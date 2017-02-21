const ONE: u8 = 0b0000_0001;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BitField8 {
    bits: u8
}

impl BitField8 {
    pub fn new() -> BitField8 {
        BitField8 { bits: 0 }
    }

    pub fn set_bit(&mut self, n: u8) {
        let mask = ONE << n;
        self.bits |= mask;
    }

    pub fn clear(&mut self, n: u8) {
        let mask = !(ONE << n);
        self.bits &= mask;
    }

    pub fn is_set(self, n: u8) -> bool {
        let mask = ONE << n;
        (self.bits & mask) != 0
    }

    pub fn any(self) -> bool {
        self.bits != 0
    }
}

#[test]
fn test_bitfield() {
    let mut bitfield = BitField8::new();

    assert!(!bitfield.any(), "bitfield should be empty");
    bitfield.set_bit(5);
    assert!(bitfield.any(), "bitfield should be non-empty");
    assert!(bitfield.is_set(5), "bit 5 should be set");
    assert!(!bitfield.is_set(1), "bit 1 should not be set");
    bitfield.clear(5);
    assert!(!bitfield.any(), "bitfield should be empty again");
    assert!(!bitfield.is_set(5), "bit 5 should not be set");
}
