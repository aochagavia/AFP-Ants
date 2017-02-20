pub use self::bitfield::BitField8;
pub use self::rng::Rng;

mod bitfield;
mod rng;

pub unsafe fn get_disjoint_mut<'a, 'b, T>(slice: &'a mut [T], i: usize) -> &'b mut T {
    assert!(i < slice.len(), "index must be within ranges");

    &mut *slice.as_mut_ptr().offset(i as isize)
}
