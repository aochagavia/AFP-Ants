use instruction::TurnDir;

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AntDirection {
    Right = 0,
    DownRight,
    DownLeft,
    Left,
    UpLeft,
    UpRight
}

impl AntDirection {
    pub fn from_u8(x: u8) -> AntDirection {
        use self::AntDirection::*;
        match x % 6 {
            0 => Right,
            1 => DownRight,
            2 => DownLeft,
            3 => Left,
            4 => UpLeft,
            5 => UpRight,
            _ => unreachable!()
        }
    }

    pub fn all() -> impl Iterator<Item=AntDirection> {
        (0...5).map(AntDirection::from_u8)
    }

    pub fn turn(self, turn_dir: TurnDir) -> AntDirection {
        match turn_dir {
            TurnDir::Left  => AntDirection::from_u8((self as u8 + 5)),
            TurnDir::Right => AntDirection::from_u8((self as u8 + 1) % 6)
        }
    }
}

pub type AntId = u16;
pub type AntState = u16; // 0..9999

#[derive(Clone, Debug, Eq, PartialEq)]
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
            direction: AntDirection::Right,
            has_food: false
        }
    }
}
