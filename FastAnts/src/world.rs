use std::io::BufRead;

use ant::{Ant, AntColor};
use util::BitField8;

// Note: the world is 0-indexed
pub struct World {
    pub cells: Vec<Cell>,
    pub width: usize,
    pub height: usize,
    red_anthill: Vec<usize>,
    black_anthill: Vec<usize>
}

impl World {
    pub fn parse<'a, R>(mut reader: R) -> World
    where R: BufRead {
        let width;
        let height;

        {
            let reader_mut = &mut reader;
            let mut parse_dimension = || {
                reader_mut.lines().next().unwrap().unwrap().trim().parse().expect("Invalid world dimension")
            };

            // The first two lines are the X and Y dimensions
            width = parse_dimension();
            height = parse_dimension();
        }

        // The rest of the lines are split into words, where each word is a cell
        let mut cells = Vec::with_capacity(width * height);
        for line in reader.lines() {
            let line = line.unwrap();
            let line = line.trim();
            let words = line.split_whitespace();
            cells.extend(words.map(Cell::parse));
        }

        World { width, height, cells, red_anthill: Vec::new(), black_anthill: Vec::new() }
    }

    // Add the ants to the world and return a vector containing their indices in ascending order of id
    pub fn populate(&mut self) -> Vec<usize> {
        let mut ants = Vec::new();

        let mut ant_id = 0;
        for (i, cell) in self.cells.iter_mut().enumerate() {
            if let Some(color) = cell.anthill {
                // Put ants on the anthills
                cell.ant = Some(Ant::new(ant_id, color));
                ants.push(i);
                ant_id += 1;

                // Save the coordinates of the anthill
                match color {
                    AntColor::Red => self.red_anthill.push(i),
                    AntColor::Black => self.black_anthill.push(i)
                }
            }
        }

        ants
    }

    pub fn count_red_food(&self) -> u32 {
        self.red_anthill.iter().map(|&i| self.cells[i].food as u32).sum()
    }

    pub fn count_black_food(&self) -> u32 {
        self.black_anthill.iter().map(|&i| self.cells[i].food as u32).sum()
    }

    pub fn count_ants(&self) -> u32 {
        self.cells.iter().filter(|cell| cell.ant.is_some()).count() as u32
    }

    pub fn count_food(&self) -> u32 {
        self.cells.iter().map(|cell| cell.food as u32).sum()
    }

    pub fn count_rocks(&self) -> u32 {
        self.cells.iter().filter(|cell| cell.is_rocky).count() as u32
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cell {
    pub ant: Option<Ant>,
    pub anthill: Option<AntColor>,
    pub is_rocky: bool,
    pub food: u16,
    pub markers_red: BitField8,
    pub markers_black: BitField8
}

impl Cell {
    pub fn markers(&self, color: AntColor) -> &BitField8 {
        match color {
            AntColor::Red => &self.markers_red,
            AntColor::Black => &self.markers_black
        }
    }

    pub fn markers_mut(&mut self, color: AntColor) -> &mut BitField8 {
        match color {
            AntColor::Red => &mut self.markers_red,
            AntColor::Black => &mut self.markers_black
        }
    }

    fn parse(word: &str) -> Cell {
        // The default cell
        let mut cell = Cell {
            ant: None,
            anthill: None,
            is_rocky: false,
            food: 0,
            markers_red: BitField8::new(),
            markers_black: BitField8::new()
        };

        // Depending on the map, the default cell is modified
        match word {
            "#" => cell.is_rocky = true,
            "." => (),
            "+" => cell.anthill = Some(AntColor::Red),
            "-" => cell.anthill = Some(AntColor::Black),
            num => cell.food = num.parse().expect("Invalid cell")
        }

        cell
    }
}
