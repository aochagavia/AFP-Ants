use std::{mem, usize};

use ant::{Ant, AntColor, AntDirection};
use instruction::{Condition, Instruction, SenseDir, TurnDir};
use util::{self, Rng};
use world::{Cell, World};

pub struct Simulator {
    pub world: World,
    red_instructions: Vec<Instruction>,
    black_instructions: Vec<Instruction>,
    pub ants: Vec<usize>,
    rng: Rng,
    round_number: u32,
    max_rounds: u32,
    food_admin: FoodAdmin
}

impl Simulator {
    pub fn new(mut world: World, red_instructions: Vec<Instruction>, black_instructions: Vec<Instruction>) -> Simulator {
        let ants = populate_world(&mut world);

        Simulator {
            world,
            red_instructions,
            black_instructions,
            ants,
            food_admin: FoodAdmin::default(),
            rng: Rng::new(12345),
            round_number: 0,
            max_rounds: 100_000
        }
    }

    // Check that the ants are in ascending order
    #[cfg(test)]
    pub fn check_asc_order(&self) {
        let get_id = |i: usize| self.world.cells[i].ant.as_ref().unwrap().id;
        let mut prev_id = get_id(self.ants[0]);
        for &ant in &self.ants[1..] {
            assert!(get_id(ant) > prev_id);
            prev_id = get_id(ant);
        }
    }

    fn one_round(&mut self) {
        assert!(self.round_number < self.max_rounds);

        self.round_number += 1;

        // For each ant, run its current instruction
        let mut ants = mem::replace(&mut self.ants, Vec::new());
        let mut position_updates = Vec::new();
        for i in 0..ants.len() {
            let ant_index = ants[i];

            // Ignore dead ants
            if ant_index == usize::MAX {
                continue;
            }

            // Get the corresponding instruction
            let instruction;
            {
                let ant = self.world.cells[ant_index].ant.as_mut().unwrap();

                if ant.resting > 0 {
                    ant.resting -= 1;
                    continue;
                }

                // Get and run the corresponding instruction
                instruction = match ant.color {
                    AntColor::Red => self.red_instructions[ant.state as usize].clone(),
                    AntColor::Black => self.black_instructions[ant.state as usize].clone()
                };
            }

            // An instruction generates position updates, which are processed afterwards
            self.run_instruction(ant_index, instruction, &mut position_updates);
            for &(old_position, new_position) in &position_updates {
                *ants.iter_mut().find(|&&mut pos| pos == old_position).unwrap() = new_position;
            }

            // Clear the position updates
            position_updates.clear();
        }

        // Restore ants to their original place
        self.ants = ants;
    }

    pub fn run(mut self) -> Outcome {
        while self.round_number < self.max_rounds {
            self.one_round();
        }

        // Note that the outcome is no longer partial, since the simulation has ended
        self.partial_outcome()
    }

    pub fn run_rounds(&mut self, rounds: u32) -> Outcome {
        for _ in 0..rounds {
            self.one_round();
        }

        self.partial_outcome()
    }

    pub fn partial_outcome(&self) -> Outcome {
        Outcome {
            red_score: self.food_admin.red_score,
            red_alive: self.world.cells.iter().filter(|cell| cell.ant.as_ref().map(|ant| ant.color) == Some(AntColor::Red)).count() as u16,
            black_score: self.food_admin.black_score,
            black_alive: self.world.cells.iter().filter(|cell| cell.ant.as_ref().map(|ant| ant.color) == Some(AntColor::Black)).count() as u16,
        }
    }

    fn run_instruction(&mut self, ant_position: usize, instruction: Instruction, position_updates: &mut Vec<(usize, usize)>) {
        // Run the instruction
        use self::Instruction::*;
        match instruction {
            Sense(dir, st1, st2, cond) => {
                // This is safe because it is the first time we index
                // WARNING: the borrow isn't tied to any lifetime!
                let cell = unsafe { util::get_disjoint_mut(&mut self.world.cells, ant_position) };
                let ant = cell.ant.as_mut().unwrap();
                let sensed_position = sensed_position(self.world.width, ant_position, ant.direction, dir);

                assert!(sensed_position != ant_position);
                // This is safe because we just checked that the indices are disjoint
                // WARNING: the borrow isn't tied to any lifetime!
                let sensed_cell = unsafe { util::get_disjoint_mut(&mut self.world.cells, sensed_position) };
                let new_state = if eval_condition(&sensed_cell, &cond, ant.color) {
                    st1
                } else {
                    st2
                };

                ant.state = new_state
            }
            Mark(mark, new_state) => {
                let cell = &mut self.world.cells[ant_position];
                let ant = cell.ant.as_mut().unwrap();
                if let AntColor::Red = ant.color {
                    cell.markers_red.set_bit(mark);
                } else {
                    cell.markers_black.set_bit(mark);
                }

                ant.state = new_state
            }
            Unmark(mark, new_state) => {
                let cell = &mut self.world.cells[ant_position];
                let ant = cell.ant.as_mut().unwrap();
                if let AntColor::Red = ant.color {
                    cell.markers_red.clear(mark);
                } else {
                    cell.markers_black.clear(mark);
                }

                ant.state = new_state
            }
            PickUp(success_state, failure_state) => {
                let cell = &mut self.world.cells[ant_position];
                let ant = cell.ant.as_mut().unwrap();
                if ant.has_food || cell.food == 0 {
                    ant.state = failure_state;
                } else {
                    self.food_admin.record_pick_up(ant.color, cell.anthill);
                    cell.food -= 1;
                    ant.has_food = true;
                    ant.state = success_state;
                }
            }
            Drop(new_state) => {
                let cell = &mut self.world.cells[ant_position];
                let ant = cell.ant.as_mut().unwrap();
                if ant.has_food {
                    self.food_admin.record_drop(ant.color, cell.anthill);
                    cell.food += 1;
                    ant.has_food = false;
                    ant.state = new_state;
                }

                ant.state = new_state;
            }
            Turn(turn_direction, new_state) => {
                let ant = self.world.cells[ant_position].ant.as_mut().unwrap();
                ant.direction = turn(ant.direction, turn_direction);
                ant.state = new_state;
            }
            Move(success_state, failure_state) => {
                let target_position;
                {
                    // This is safe because it is the first time we index
                    // WARNING: the borrow isn't tied to any lifetime!
                    let cell = unsafe { util::get_disjoint_mut(&mut self.world.cells, ant_position) };
                    {
                        let ant = cell.ant.as_mut().unwrap();
                        target_position = adjacent_position
                    (self.world.width, ant_position, ant.direction);
                    }
                    assert!(target_position != ant_position);

                    // This is safe because we just checked that the indices are disjoint
                    // WARNING: the borrow isn't tied to any lifetime!
                    let target_cell = unsafe { util::get_disjoint_mut(&mut self.world.cells, target_position) };

                    // Stop here if the target cell is occupied
                    if target_cell.is_rocky || target_cell.ant.is_some() {
                        let ant = cell.ant.as_mut().unwrap();
                        ant.state = failure_state;
                        return;
                    }

                    // Remove ant from current place and add it to the new one
                    target_cell.ant = cell.ant.take();

                    // Update the position of this ant
                    position_updates.push((ant_position, target_position));

                    // Don't forget to rest and update the state
                    let ant = target_cell.ant.as_mut().unwrap();
                    ant.resting = 14;
                    ant.state = success_state;
                }

                self.kill_surrounded_ants(target_position, position_updates);
            }
            Flip(n, st1, st2) => {
                let ant = self.world.cells[ant_position].ant.as_mut().unwrap();
                let random = self.rng.random_int(n as usize);
                let new_state = if random == 0 { st1 } else { st2 };
                ant.state = new_state;
            }
        }
    }

    fn kill_surrounded_ants(&mut self, position: usize, position_updates: &mut Vec<(usize, usize)>) {
        // Check if this ant is surrounded
        let mut this_ant_dead = false;
        if let Some(ref this_ant) = self.world.cells[position].ant {
            this_ant_dead = self.adjacent_enemies(position, this_ant.color) >= 5;
        }

        if this_ant_dead {
            self.handle_killed_ant(position, position_updates);
        }

        // For each adjacent cell, check if there is a surrounded ant
        for direction in AntDirection::all() {
            let cell_position = adjacent_position(self.world.width, position, direction);
            let mut ant_dead = false;
            if let Some(ref ant) = self.world.cells[cell_position].ant {
                ant_dead = self.adjacent_enemies(cell_position, ant.color) >= 5;
            }

            if ant_dead {
                self.handle_killed_ant(cell_position, position_updates);
            }
        }
    }

    fn handle_killed_ant(&mut self, position: usize, position_updates: &mut Vec<(usize, usize)>) {
        // Remove from cell'
        let cell = &mut self.world.cells[position];
        let ant = cell.ant.take().unwrap();

        // Set the position to an invalid value
        position_updates.push((position, usize::MAX));

        // Drop food
        self.food_admin.record_dead(ant, cell.anthill);
    }

    fn adjacent_enemies(&self, position: usize, friend_color: AntColor) -> usize {
        AntDirection::all()
               .map(|dir| adjacent_position(self.world.width, position, dir))
               .filter_map(|i| self.world.cells[i].ant.as_ref())
               .filter(|ant| ant.color == friend_color)
               .count()
    }
}

fn turn(ant_dir: AntDirection, turn_dir: TurnDir) -> AntDirection {
    match turn_dir {
        TurnDir::Left  => ant_dir.turn_left(),
        TurnDir::Right => ant_dir.turn_right()
    }
}

fn eval_condition(cell: &Cell, cond: &Condition, color: AntColor) -> bool {
    use self::Condition::*;
    match (*cond, &cell.ant) {
        (Rock          , _             ) => cell.is_rocky,
        (Food          , _             ) => cell.food > 0,
        (Home          , _             ) => cell.anthill == Some(color),
        (FoeHome       , _             ) => cell.anthill == Some(color.enemy()),
        (Marker(i)     , _             ) => cell.markers(color).is_set(i),
        (FoeMarker     , _             ) => cell.markers(color.enemy()).any(),
        (_             , &None         ) => false,
        (Friend        , &Some(ref ant)) => ant.color == color,
        (Foe           , &Some(ref ant)) => ant.color != color,
        (FriendWithFood, &Some(ref ant)) => ant.color == color && ant.has_food,
        (FoeWithFood   , &Some(ref ant)) => ant.color != color && ant.has_food,
    }
}

pub fn adjacent_position(width: usize, i: usize, dir: AntDirection) -> usize {
    // Note: There is no out of bounds, since the cells of the perimeter are always rocky

    // FIXME: not sure whether this is correct
    let (x, y) = (i as isize % width as isize, i as isize / width as isize);

    use self::AntDirection::*;
    let (dx, dy) = match dir {
        Right                   => (1, 0),
        DownRight if y % 2 == 0 => (0, 1),
        DownRight               => (1, 1),
        DownLeft  if y % 2 == 0 => (-1, 1),
        DownLeft                => (0, 1),
        Left                    => (-1, 0),
        UpLeft    if y % 2 == 0 => (-1, -1),
        UpLeft                  => (0, -1),
        UpRight   if y % 2 == 0 => (0, -1),
        UpRight                 => (1, -1),
    };

    let new_x = x + dx;
    let new_y = y + dy;

    // FIXME: not sure whether this is correct
    new_y as usize * width + new_x as usize
}

fn sensed_position(width: usize, ant_index: usize, ant_direction: AntDirection, sense_direction: SenseDir) -> usize {
    use self::SenseDir::*;
    match sense_direction {
        Here       => ant_index,
        Ahead      => adjacent_position
    (width, ant_index, ant_direction),
        LeftAhead  => adjacent_position
    (width, ant_index, turn(ant_direction, TurnDir::Left)),
        RightAhead => adjacent_position
    (width, ant_index, turn(ant_direction, TurnDir::Right))
    }
}

// Add the ants to the world and return a vector containing their indices in ascending order of id
fn populate_world(world: &mut World) -> Vec<usize> {
    let mut ants = Vec::new();
    let mut ant_id = 0;
    for (i, cell) in world.cells.iter_mut().enumerate() {
        if let Some(color) = cell.anthill {
            cell.ant = Some(Ant::new(ant_id, color));
            ants.push(i);
            ant_id += 1;
        }
    }

    ants
}

#[derive(Default)]
struct FoodAdmin {
    red_score: u32,
    black_score: u32,
    black_carried: u32,
    red_carried: u32,
}

impl FoodAdmin {
    fn record_pick_up(&mut self, color: AntColor, anthill: Option<AntColor>) {
        *self.carried(color) += 1;
        self.with_score(anthill, |s| s - 1);
    }

    fn record_drop(&mut self, color: AntColor, anthill: Option<AntColor>) {
        *self.carried(color) -= 1;
        self.with_score(anthill, |s| s + 1);
    }

    fn record_dead(&mut self, ant: Ant, anthill: Option<AntColor>) {
        if ant.has_food {
            *self.carried(ant.color) -= 1;
            self.with_score(anthill, |s| s + 1);
        }

        self.with_score(anthill, |s| s + 3);
    }

    fn carried(&mut self, color: AntColor) -> &mut u32 {
        match color {
            AntColor::Red => &mut self.red_carried,
            AntColor::Black => &mut self.black_carried
        }
    }

    fn with_score<F>(&mut self, anthill: Option<AntColor>, f: F)
    where F: Fn(u32) -> u32 {
        match anthill {
            Some(AntColor::Red) => self.red_score = f(self.red_score),
            Some(AntColor::Black) => self.black_score = f(self.black_score),
            None => ()
        }
    }
}

#[derive(Debug)]
pub struct Outcome {
    pub red_score: u32,
    pub red_alive: u16,
    pub black_score: u32,
    pub black_alive: u16
}
