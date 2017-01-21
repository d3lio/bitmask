//! A bitmask generator for enum scoped bit flags.
//!
//! **Disclaimer:** For a more mature module check out [bitflags!](https://crates.io/crates/bitflags)!!
//!
//! ---
//!
//! With that out of the way lets check out what this baby can do!

/// A macro that generates a bitmask and it's associated bit flags.
///
/// The `bitmask!` macro creates a struct and an enum that hold your flags. The enum contains all the
/// bit flag variants and the struct is a mixture of those bit flags called a bitmask.
/// It's syntax is as follows:
///
/// ```ignore
/// bitmask! {
///     [pub] mask <struct_name>: <struct_type> where <enum_name> {
///         <flag_name>: <value>,
///         ...
///     }
/// }
/// ```
///
/// where `struct_type` can be one of the primitive integer types (`i8-64`, `u8-64`, `isize`, `usize`).
///
/// If for some reason you want to define the enum and the struct yourself you can do so and use the
/// `@IMPL` branch of the macro to implement the methods. The only restrictions are that your
/// struct's inner field must be named `mask` and the enum should have the same size as the struct
/// which can be achieved through the `#[repr()]` modifier with the same integer type as the field `mask`.
///
/// You can implement `Into<struct_name>` and `Deref` for your own custom type if you want to
/// use it with the preimplemented methods for the mask but beware that this can change with time and
/// does not apply for the trait impls like `BitOr` for example.
///
/// # Examples:
///
/// ```
/// # #[macro_use] extern crate bitmask; fn main() {
/// bitmask! {
///     mask BitMask: u32 where Flags {
///         Flag1       = 0x00000001,
///         Flag2       = 0x000000F0,
///         Flag3       = 0x00000800,
///         Flag123     = 0x000008F1,
///         // Note that function calls like `isize::min_value()`
///         // can't be used for enum discriminants in Rust.
///         FlagMax     = ::std::u32::MAX
///     }
///     // This mask is to show that you can create multiple masks in a single macro invocation
///     // as long as they have the same visibility (priv/pub).
///     mask SomeOtherMask: isize where SomeOtherFlags {
///         FlagZero    = 0,
///         FlagOne     = 1
///     }
/// }
///
/// let mut mask = BitMask::new();
///
/// mask.set(Flags::Flag1 | Flags::Flag2);
/// assert_eq!(*mask, 0x000000F1);
///
/// mask.unset(Flags::Flag1);
/// assert_eq!(*mask, 0x000000F0);
///
/// mask.set(Flags::Flag123);
/// assert_eq!(*mask, 0x000008F1);
/// # }
/// ```
///
/// Maybe not the best example but still... I like caaaake
///
/// ```
/// # #[macro_use] extern crate bitmask; fn main() {
/// bitmask! {
///     mask Cake: u8 where Ingredients {
///         Sugar   = 0b00000001,
///         Eggs    = 0b00000010,
///         Flour   = 0b00000100,
///         Milk    = 0b00001000
///     }
/// }
///
/// let quality_cake = Cake::all();
/// assert_eq!(*quality_cake, 0b00001111);
/// # }
/// ```
// TODO: Add meta attributes when this error is gone
// `local ambiguity: multiple parsing options: built-in NTs ident ('flag_name') or 1 other option.`
// I have no idea why #[<meta>] can have local ambiguity with `ident`...
#[macro_export]
macro_rules! bitmask {
    ($($(#[$st_attr: meta])* pub mask $st_name: ident : $T: tt where $en_name: ident {
        $($flag_name: ident = $flag_val: expr),+
    })*) => {
        $(
            #[repr($T)]
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            pub enum $en_name {
                $($flag_name = $flag_val),+
            }
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            $(#[$st_attr])*
            pub struct $st_name {
                mask: $T
            }
            bitmask!(@IMPL $st_name $T $en_name, {
                $($flag_name = $flag_val),+
            });
        )*
    };

    ($($(#[$st_attr: meta])* mask $st_name: ident : $T: tt where $en_name: ident {
        $($flag_name: ident = $flag_val: expr),+
    })*) => {
        $(
            #[repr($T)]
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            enum $en_name {
                $($flag_name = $flag_val),+
            }
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            $(#[$st_attr])*
            struct $st_name {
                mask: $T
            }
            bitmask!(@IMPL $st_name $T $en_name, {
                $($flag_name = $flag_val),+
            });
        )*
    };

    ($($(#[$st_attr: meta])* pub mask $st_name: ident : $T: tt where $en_name: ident {
        $($flag_name: ident = $flag_val: expr),+,
    })*) => {
        $(
            #[repr($T)]
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            pub enum $en_name {
                $($flag_name = $flag_val),+
            }
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            $(#[$st_attr])*
            pub struct $st_name {
                mask: $T
            }
            bitmask!(@IMPL $st_name $T $en_name, {
                $($flag_name = $flag_val),+
            });
        )*
    };

    ($($(#[$st_attr: meta])* mask $st_name: ident : $T: tt where $en_name: ident {
        $($flag_name: ident = $flag_val: expr),+,
    })*) => {
        $(
            #[repr($T)]
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            enum $en_name {
                $($flag_name = $flag_val),+
            }
            #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[allow(dead_code)]
            $(#[$st_attr])*
            struct $st_name {
                mask: $T
            }
            bitmask!(@IMPL $st_name $T $en_name, {
                $($flag_name = $flag_val),+
            });
        )*
    };

    (@IMPL $st_name: ident $T: tt $en_name: ident, {
        $($flag_name: ident = $flag_val: expr),+
    }) => {
        #[allow(dead_code)]
        impl $st_name {
            /// Create a new mask with all flags unset.
            #[inline]
            pub fn new() -> Self {
                $st_name {
                    mask: 0
                }
            }

            /// Create a new mask with all flags set.
            #[inline]
            pub fn all() -> Self {
                $st_name {
                    mask: $($flag_val)|+
                }
            }

            /// Set all flags that `other` contains.
            ///
            /// `other` can be either a single flag or another mask.
            #[inline]
            pub fn set<T>(&mut self, other: T)
                where T: Into<$st_name> + ::std::ops::Deref<Target = $T> {
                    self.mask |= *other;
            }

            /// Unset all flags that `other` contains.
            ///
            /// `other` can be either a single flag or another mask.
            #[inline]
            pub fn unset<T>(&mut self, other: T)
                where T: Into<$st_name> + ::std::ops::Deref<Target = $T> {
                    self.mask &= Self::all().mask ^ *other;
            }

            /// Toggle all flags that `other` contains.
            ///
            /// `other` can be either a single flag or another mask.
            #[inline]
            pub fn toggle<T>(&mut self, other: T)
                where T: Into<$st_name> + ::std::ops::Deref<Target = $T> {
                    self.mask ^= *other;
            }

            /// Check if the mask contains all flags that `other` contains.
            ///
            /// `other` can be either a single flag or another mask.
            #[inline]
            pub fn contains<T>(&mut self, other: T) -> bool
                where T: Into<$st_name> + ::std::ops::Deref<Target = $T> {
                    self.mask & *other == *other
            }

            /// Toggle all flags that other contains.
            ///
            /// `other` can be either a single flag or another mask.
            #[inline]
            pub fn intersects<T>(&mut self, other: T) -> bool
                where T: Into<$st_name> + ::std::ops::Deref<Target = $T> {
                    self.mask & *other != 0
            }

            /// Check if all flags are set.
            pub fn is_all(&self) -> bool {
                self.mask == Self::all().mask
            }

            /// Check if all flags are set.
            pub fn is_none(&self) -> bool {
                self.mask == 0
            }
        }

        impl ::std::convert::From<$en_name> for $st_name {
            /// Create a mask from a single flag.
            ///
            /// When creating a mask from multiple flags or another mask just use the `clone` method
            /// or the `copy` semantics.
            #[inline]
            fn from(flag: $en_name) -> Self {
                $st_name {
                    mask: flag as $T
                }
            }
        }
        impl ::std::ops::Deref for $st_name {
            type Target = $T;
            /// Deref to the internal type.
            ///
            /// Useful for FFI.
            #[inline]
            fn deref(&self) -> &$T {
                &self.mask as &$T
            }
        }
        impl ::std::ops::Deref for $en_name {
            type Target = $T;
            /// Deref to the internal type.
            ///
            /// Useful for FFI.
            #[inline]
            fn deref(&self) -> &$T {
                unsafe { ::std::mem::transmute(self) }
            }
        }

        // TODO: when `concat_idents!` is stable, replace the `IMPL`s with a single impl on a common trait
        // and use static dispatch. Right now using dispatch is out of the question because it would
        // have the traits be implemented for all types that deref to `$T`. `T: Into + Deref`
        // cannot be used in `impl` since the compiler thinks that's too generic.

        bitmask! { @IMPL_BITOR
            $st_name, $st_name, $st_name;
            $st_name, $en_name, $st_name;
            $en_name, $st_name, $st_name;
            $en_name, $en_name, $st_name;
        }
        bitmask! { @IMPL_BITAND
            $st_name, $st_name, $st_name;
            $st_name, $en_name, $st_name;
            $en_name, $st_name, $st_name;
            $en_name, $en_name, $st_name;
        }
        bitmask! { @IMPL_BITXOR
            $st_name, $st_name, $st_name;
            $st_name, $en_name, $st_name;
            $en_name, $st_name, $st_name;
            $en_name, $en_name, $st_name;
        }
        bitmask! { @IMPL_BITOR_ASSIGN
            $st_name, $st_name;
            $st_name, $en_name;
        }
        bitmask! { @IMPL_BITAND_ASSIGN
            $st_name, $st_name;
            $st_name, $en_name;
        }
        bitmask! { @IMPL_BITXOR_ASSIGN
            $st_name, $st_name;
            $st_name, $en_name;
        }
        bitmask! { @IMPL_NOT
            $st_name, $st_name;
            $en_name, $st_name;
        }
    };

    (@IMPL_BITOR $($target: ty, $other: ty, $st_name: ident);*;) => {
        $(impl ::std::ops::BitOr<$other> for $target {
            type Output = $st_name;
            #[inline]
            fn bitor(self, other: $other) -> Self::Output {
                $st_name {
                    mask: *self | *other
                }
            }
        })*
    };

    (@IMPL_BITAND $($target: ty, $other: ty, $st_name: ident);*;) => {
        $(impl ::std::ops::BitAnd<$other> for $target {
            type Output = $st_name;
            #[inline]
            fn bitand(self, other: $other) -> Self::Output {
                $st_name {
                    mask: *self & *other
                }
            }
        })*
    };

    (@IMPL_BITXOR $($target: ty, $other: ty, $st_name: ident);*;) => {
        $(impl ::std::ops::BitXor<$other> for $target {
            type Output = $st_name;
            #[inline]
            fn bitxor(self, other: $other) -> Self::Output {
                $st_name {
                    mask: *self ^ *other
                }
            }
        })*
    };

    (@IMPL_BITOR_ASSIGN $($target: ty, $other: ty);*;) => {
        $(impl ::std::ops::BitOrAssign<$other> for $target {
            #[inline]
            fn bitor_assign(&mut self, other: $other) {
                self.mask |= *other
            }
        })*
    };

    (@IMPL_BITAND_ASSIGN $($target: ty, $other: ty);*;) => {
        $(impl ::std::ops::BitAndAssign<$other> for $target {
            #[inline]
            fn bitand_assign(&mut self, other: $other) {
                self.mask &= *other
            }
        })*
    };

    (@IMPL_BITXOR_ASSIGN $($target: ty, $other: ty);*;) => {
        $(impl ::std::ops::BitXorAssign<$other> for $target {
            #[inline]
            fn bitxor_assign(&mut self, other: $other) {
                self.mask ^= *other
            }
        })*
    };

    (@IMPL_NOT $($target: ty, $st_name: ident);*;) => {
        $(impl ::std::ops::Not for $target {
            type Output = $st_name;
            #[inline]
            fn not(self) -> Self::Output {
                let all_flags = $st_name::all();
                $st_name {
                    mask: *all_flags ^ *self
                }
            }
        })*
    }
}

#[cfg(test)]
mod tests {
    bitmask! {
        mask BitMask: isize where Flags {
            Flag1      = 0b00000001,
            Flag2      = 0b00000010,
            Flag3      = 0b00000100,
            FlagMin    = ::std::isize::MIN,
            Flag123    = 0b00000111
        }
        /// Doc comment struct
        mask U16: u16 where FlagsU16 {
            F1 = 1
        }
    }
    bitmask! {
        mask A: u8 where B {
            Trailing = 1,
        }
    }
    use self::Flags::*;

    #[test]
    fn test_set_unset() {
        let mut bm = BitMask::new();
        assert_eq!(*bm, 0b00000000);
        bm.set(Flag2);
        assert_eq!(*bm, Flag2 as isize);
        bm.set(Flag3);
        assert_eq!(*bm, 0b00000110);
        bm.unset(Flag123);
        assert_eq!(*bm, 0b00000000);
        bm.set(FlagMin);
        assert_eq!(*bm, isize::min_value());
    }

    #[test]
    fn test_toggle() {
        let mut bm = BitMask::new();
        bm.toggle(Flag2);
        assert_eq!(*bm, 0b00000010);
        bm.toggle(BitMask::from(Flag3));
        assert_eq!(*bm, 0b00000110);
        bm.toggle(Flag123);
        assert_eq!(*bm, 0b00000001);
    }

    #[test]
    fn test_contains() {
        let mut bm = BitMask::from(Flag2 | Flag3);
        assert_eq!(bm.contains(Flag1), false);
        assert_eq!(bm.contains(Flag2), true);
        assert_eq!(bm.contains(Flag3), true);
        assert_eq!(bm.contains(Flag123), false);
        bm.set(Flag123);
        assert_eq!(bm.contains(Flag123), true);
        bm.set(FlagMin);
        assert_eq!(bm.contains(Flag123), true);
    }

    #[test]
    fn test_intersects() {
        let mut bm = BitMask::from(Flag2 | Flag3);
        assert_eq!(bm.intersects(Flag1), false);
        assert_eq!(bm.intersects(Flag2), true);
        assert_eq!(bm.intersects(Flag3), true);
        assert_eq!(bm.intersects(Flag1 | Flag3), true);
        assert_eq!(bm.intersects(Flag123), true);
    }

    #[test]
    fn test_is_all() {
        assert_eq!(BitMask::all().is_all(), true);
        assert_eq!(BitMask::new().is_all(), false);
        assert_eq!(BitMask::from(Flag1).is_all(), false);
        assert_eq!(BitMask::from(Flag123 | FlagMin).is_all(), true);
    }

     #[test]
    fn test_is_none() {
        assert_eq!(BitMask::all().is_none(), false);
        assert_eq!(BitMask::new().is_none(), true);
        assert_eq!(BitMask::from(Flag1).is_none(), false);
        assert_eq!(BitMask::from(Flag123 | FlagMin).is_none(), false);
    }

    #[test]
    fn test_bitor() {
        let bm = Flag1 | Flag3;
        assert_eq!(*bm, 0b00000101);

        let bm = BitMask::from(Flag1) | BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000101);

        let bm = Flag1 | BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000101);

        let bm = BitMask::from(Flag1) | Flag3;
        assert_eq!(*bm, 0b00000101);
    }

    #[test]
    fn test_bitand() {
        let bm = Flag1 & Flag3;
        assert_eq!(*bm, 0);

        let bm = BitMask::from(Flag1) & BitMask::from(Flag3);
        assert_eq!(*bm, 0);

        let bm = Flag1 & BitMask::from(Flag3);
        assert_eq!(*bm, 0);

        let bm = BitMask::from(Flag1) & Flag3;
        assert_eq!(*bm, 0);
    }

    #[test]
    fn test_bitxor() {
        let bm = Flag123 ^ Flag3;
        assert_eq!(*bm, 0b00000011);

        let bm = BitMask::from(Flag123) ^ BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000011);

        let bm = Flag123 ^ BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000011);

        let bm = BitMask::from(Flag123) ^ Flag3;
        assert_eq!(*bm, 0b00000011);
    }

    #[test]
    fn test_bitor_assign() {
        let mut bm = BitMask::from(Flag1);
        bm |= Flag3;
        assert_eq!(*bm, 0b00000101);

        let mut bm = BitMask::from(Flag1);
        bm |= BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000101);
    }

    #[test]
    fn test_bitand_assign() {
        let mut bm = BitMask::from(Flag1);
        bm &= Flag3;
        assert_eq!(*bm, 0);

        let mut bm = BitMask::from(Flag1);
        bm &= BitMask::from(Flag3);
        assert_eq!(*bm, 0);
    }

    #[test]
    fn test_bitxor_assign() {
        let mut bm = BitMask::from(Flag123);
        bm ^= Flag3;
        assert_eq!(*bm, 0b00000011);

        let mut bm = BitMask::from(Flag123);
        bm ^= BitMask::from(Flag3);
        assert_eq!(*bm, 0b00000011);
    }

    #[test]
    fn test_bitnot() {
        let res = !Flag2;
        assert_eq!(*res, isize::min_value() + 0b00000101);

        let res = !BitMask::from(Flag1);
        assert_eq!(*res, isize::min_value() + 0b00000110);
    }

    #[test]
    fn test_pub_mask() {
        mod inner {
            bitmask! {
                pub mask InnerMask: u8 where InnerFlags {
                    InnerFlag1 = 0
                }
            }
        }
        let _ = inner::InnerMask::new();
        let _ = inner::InnerFlags::InnerFlag1;
    }
}
