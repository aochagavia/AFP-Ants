use std::io::BufRead;

use ant::{AntColor, AntState};
use world::Cell;

pub type MarkerNumber = u8; // 0..5
pub type InvChance = u16; // 1.. (1 / 1 == 100%, 1 / 2 == 50%, 1 / 3 == 33%)

#[derive(Clone)]
pub enum Instruction {
    Sense(SenseDir, AntState, AntState, Condition),
    Mark(MarkerNumber, AntState),
    Unmark(MarkerNumber, AntState),
    PickUp(AntState, AntState),
    Drop(AntState),
    Turn(TurnDir, AntState),
    Move(AntState, AntState),
    Flip(InvChance, AntState, AntState)
}

#[derive(Clone, Copy)]
pub enum SenseDir {
    Here,
    Ahead,
    LeftAhead,
    RightAhead
}

#[derive(Clone, Copy)]
pub enum TurnDir {
    Left,
    Right
}

#[derive(Clone, Copy)]
pub enum Condition {
    Friend,
    Foe,
    FriendWithFood,
    FoeWithFood,
    Food,
    Rock,
    Marker(MarkerNumber),
    FoeMarker,
    Home,
    FoeHome
}

impl Condition {
    pub fn eval(&self, cell: &Cell, color: AntColor) -> bool {
        use self::Condition::*;
        match (*self, &cell.ant) {
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
}

pub fn parse<R>(reader: R) -> Vec<Instruction>
where R: BufRead {
    let mut instrs = Vec::new();
    for line in reader.lines() {
        let line = line.unwrap();
        let line = line.trim();
        if line.is_empty() {
            break;
        }

        instrs.push(parse_instruction(&line.to_uppercase()));
    }

    assert!(instrs.len() < 10_000, "Too many states");

    instrs
}

fn parse_instruction(s: &str) -> Instruction {
    use self::Instruction::*;
    let words: Vec<_> = s.split_whitespace().collect();
    match words[..] {
        ["SENSE", sensedir, st1, st2, cond] =>
            Sense(parse_sense_dir(sensedir), parse_state(st1), parse_state(st2), parse_condition(cond)),
        ["SENSE", sensedir, st1, st2, "MARKER", i] =>
            Sense(parse_sense_dir(sensedir), parse_state(st1), parse_state(st2), Condition::Marker(parse_marker(i))),
        ["MARK", i, st] => Mark(parse_marker(i), parse_state(st)),
        ["UNMARK", i, st] => Unmark(parse_marker(i), parse_state(st)),
        ["PICKUP", st1, st2] => PickUp(parse_state(st1), parse_state(st2)),
        ["DROP", st] => Drop(parse_state(st)),
        ["TURN", lr, st] => Turn(parse_turn_dir(lr), parse_state(st)),
        ["MOVE", st1, st2] => Move(parse_state(st1), parse_state(st2)),
        ["FLIP", p, st1, st2] => Flip(p.parse().unwrap(), parse_state(st1), parse_state(st2)),
        _ => panic!("Unknown instruction: {}", s)
    }
}

fn parse_state(s: &str) -> AntState {
    s.parse().into_iter().filter(|&x| x < 10_000).next().expect("Invalid AntState")
}

fn parse_marker(s: &str) -> MarkerNumber {
    // NOTE: the haskell code uses modulo to handle overflow. We panic instead.
    s.parse().into_iter().filter(|&x| x < 6).next().expect("Invalid MarkerNumber")
}

fn parse_sense_dir(s: &str) -> SenseDir {
    use self::SenseDir::*;
    match s {
        "HERE"       => Here,
        "AHEAD"      => Ahead,
        "LEFTAHEAD"  => LeftAhead,
        "RIGHTAHEAD" => RightAhead,
        _            => panic!("Invalid SenseDir: {}", s)
    }
}

fn parse_turn_dir(s: &str) -> TurnDir {
    match s {
        "LEFT"  => TurnDir::Left,
        "RIGHT" => TurnDir::Right,
        _       => panic!("Invalid TurnDir: {}", s)
    }
}

fn parse_condition(s: &str) -> Condition {
    use self::Condition::*;
    match s {
        "FRIEND"         => Friend,
        "FOE"            => Foe,
        "FRIENDWITHFOOD" => FriendWithFood,
        "FOEWITHFOOD"    => FoeWithFood,
        "FOOD"           => Food,
        "ROCK"           => Rock,
        "FOEMARKER"      => FoeMarker,
        "HOME"           => Home,
        "FOEHOME"        => FoeHome,
        _                => panic!("Invalid condition: {}", s)
    }
}
