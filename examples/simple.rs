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

fn main() {
    use sub::{BitMask, Flags};

    let mut mask1 = BitMask::none();
    mask1.set(Flags::Flag1);

    let mask2 = BitMask::from(Flags::Flag2 | Flags::Flag3);

    // Prints false since mask1 and mask2 don't have common flags
    println!("{:?}", mask1.intersects(mask2));
}
