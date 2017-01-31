#[macro_use]
extern crate bitmask;

mod sub {
    bitmask! {
        pub mask BitMask: u32 where flags Flags {
            Flag1 = 1,
            Flag2 = 2,
            Flag3 = 4,
            Flag4 = 8
        }
    }
}

use sub::BitMask;

fn features1(options: BitMask) {
    // Some logic here...
    println!("{:?}", options);
}

// If a type can `into` BitMask then it's either BitMask itself, a variant of the Flags enum or
// some custom type you designed to work with the mask.
fn features2<T: Into<BitMask>>(_: T) {
    // Some logic here...
}

fn main() {
    use sub::Flags::*;

    features1(Flag3.into());
    features1(Flag1 | Flag4);

    features2(Flag3);
    features2(Flag1 | Flag4);
}
