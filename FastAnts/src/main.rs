#![feature(conservative_impl_trait, inclusive_range_syntax, slice_patterns)]

mod ant;
mod instruction;
mod simulator;
mod util;
mod world;

#[cfg(test)]
mod test;

use std::io;
use simulator::Simulator;

fn main() {
    // Boilerplate to read lines from stdin
    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();

    // First, read the world
    let world = world::parse(&mut stdin_lock);

    // Then the instructions for the ants
    let red = instruction::parse(&mut stdin_lock);
    let black = instruction::parse(&mut stdin_lock);

    let mut simulator = Simulator::new(world, red, black);
    simulator.run();
}
