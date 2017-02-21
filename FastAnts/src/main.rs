#![feature(conservative_impl_trait, inclusive_range_syntax, slice_patterns)]

mod ant;
mod instruction;
mod simulator;
mod test_data;
mod util;
mod world;

#[cfg(test)]
mod test;

//use std::io;
use simulator::Simulator;
//use world::World;

fn main() {
    // Boilerplate to read lines from stdin
    /*
    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();

    // First, read the world
    let world = World::parse(&mut stdin_lock);

    // Then the instructions for the ants
    let red = instruction::parse(&mut stdin_lock);
    let black = instruction::parse(&mut stdin_lock);
    */

    let world = test_data::sample0();
    let instr = test_data::default_program();

    let simulator = Simulator::new(world, instr.clone(), instr);
    println!("{:?}", simulator.run());
}
