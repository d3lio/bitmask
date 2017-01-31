#[macro_use]
extern crate bitmask;

bitmask! {
    pub mask BitMask: u32 where flags Flags {
        Flag1 = 1,
        Flag2 = 2,
        Flag3 = 4,
        Flag4 = 8
    }
}

impl BitMask {
    /// Difference between `self` and `other`
    ///
    /// Can take a flag or another bitmask as `other`.
    #[inline]
    pub fn difference<T>(&mut self, other: T) -> Self
        where T: Into<Self> + ::std::ops::Deref<Target = u32> {
            BitMask {
                mask: self.mask & (Self::all().mask ^ *other)
            }
    }
}

impl Flags {
    /// Example method
    pub fn say<T>(msg: T) where T: ::std::borrow::Borrow<str> + ::std::fmt::Display {
        println!("{}", msg);
    }
}

fn main() {
    use Flags::*;

    // These two are the same
    let mut mask1 = BitMask::from(Flag1);
    let mut mask2: BitMask = Flag1.into();

    mask1 |= Flag2;
    mask2 |= Flag4;

    assert_eq!(*mask1.difference(mask2), *Flag2);

    Flags::say("This has nothing to do with flags...");
}
