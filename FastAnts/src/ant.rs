#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AntColor {
    Red,
    Black
}

impl AntColor {
    pub fn enemy(self) -> AntColor {
        match self {
            AntColor::Red => AntColor::Black,
            AntColor::Black => AntColor::Red
        }
    }
}

pub type AntDirection = u8;
pub type AntId = u16;
pub type AntState = u16; // 0..9999

#[derive(Clone, Debug)]
pub struct Ant {
    pub color: AntColor,
    pub id: AntId,
    pub state: AntState,
    pub resting: u8,
    pub direction: AntDirection, // From 0 to 5, since the world is hexagonal
    pub has_food: bool
}

impl Ant {
    pub fn new(id: AntId, color: AntColor) -> Ant {
        Ant {
            color,
            id,
            state: 0,
            resting: 0,
            direction: 0,
            has_food: false
        }
    }
}
