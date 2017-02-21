use ant::AntDirection;
use simulator::{adjacent_position, Simulator};
use test_data::{ant1, default_program, sample0};

#[test]
fn test_parse_instructions() {
    let instrs = ant1();
    assert_eq!(instrs.len(), 2333)
}

#[test]
fn test_parse_world() {
    let world = sample0();
    assert_eq!(world.width, 100);
    assert_eq!(world.height, 100);
    assert_eq!(world.cells.len(), 10_000);
    assert_eq!(world.count_food(), 480);
    assert_eq!(world.count_rocks(), 850);
}

#[test]
fn test_adjacent_position() {
    use self::AntDirection::*;
    let width = 100;
    // One step to the right
    assert_eq!(adjacent_position(width, 0, Right), 1);

    // One step to the right, from (0, 2)
    assert_eq!(adjacent_position(width, 2 * width, Right), 2 * width + 1);

    // One step to the left
    assert_eq!(adjacent_position(width, width - 1, Left), width - 2);

    // One step to the left, from (1, 2)
    assert_eq!(adjacent_position(width, 2 * width + 1, Left), 2 * width);

    // One step up-right, from (0, 1)
    assert_eq!(adjacent_position(width, width, UpRight), 1);

    // One step up-right, from (0, 2)
    assert_eq!(adjacent_position(width, 2 * width, UpRight), width);

    // Other possible tests: up-left, down-right, down-left
}

#[test]
fn test_create_simulator() {
    let world = sample0();
    let simulator = Simulator::new(world, ant1(), ant1());
    assert_eq!(simulator.ants.len(), 182);

    // The ids are stored in ascending order in the positions vector
    let get_id = |i: usize| simulator.world.cells[i].ant.as_ref().unwrap().id;
    let mut prev_id = get_id(simulator.ants[0]);
    for &ant in &simulator.ants[1..] {
        assert!(get_id(ant) > prev_id);
        prev_id = get_id(ant);
    }
}

#[test]
fn test_run_simulator() {
    let world = sample0();
    let instr = default_program();
    let mut simulator = Simulator::new(world, instr.clone(), instr);
    assert_eq!(simulator.world.count_ants(), 182);
    let outcome = simulator.run_rounds(10_000);
    assert_eq!(outcome.red_score, 0);
    assert_eq!(outcome.black_score, 7);
}
