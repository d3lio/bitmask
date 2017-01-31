#[macro_use]
extern crate bitmask;

bitmask! {
    mask BitMask: u32 where flags Flags {
        Flag1 = 1,
        Flag2 = 2,
        Flag3 = 4,
        Flag4 = 8
    }
}

struct FakeMask {
    mask: u32
}

impl ::std::ops::Deref for FakeMask {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.mask
    }
}

impl ::std::convert::From<FakeMask> for BitMask {
    fn from(fake: FakeMask) -> Self {
        BitMask {
            mask: fake.mask
        }
    }
}

fn main() {
    use Flags::*;

    let fake = FakeMask {
        mask: Flag1 as u32
    };

    let mask: BitMask = fake.into();

    assert_eq!(mask.mask, 1);
}
