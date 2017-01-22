//! A bitmask generator for enum scoped bit flags.
//!
//! For a more mature crate be sure to check out [bitflags!](https://crates.io/crates/bitflags)!!
//!
//! With that out of the way lets check out what this baby can do!

/// A macro that generates a bitmask and it's associated bit flags.
///
/// The `bitmask!` macro creates a struct and an enum that holds your flags. The enum contains all the
/// bit flag variants and the struct is a mixture of those bit flags called a bitmask.
/// It's syntax is as follows:
///
/// ```ignore
/// bitmask! {
///     [pub] mask <struct_name>: <struct_type> where flags <enum_name> {
///         <flag_name> = <value>,
///         ...
///     }
/// }
/// ```
///
/// where `struct_type` can be one of the primitive integer types (`i8-64`, `u8-64`, `isize`, `usize`).
///
/// Sometimes you might want to wrap some lib that ports `C` or some other code through FFI
/// which exposes numerous defines/constants as `const`. For example if you had this module:
///
/// ```ignore
/// mod tex {
///     ...
///     pub const TEXTURE_2D: u32   = 1;
///     pub const TEXTURE_3D: u32   = 2;
///     pub const FLIP: u32         = 4;
///     ...
///     pub fn set_options(mask: u32) { ... }
/// }
/// ```
///
/// To avoid collisions you would use these through the mod scope like so:
///
/// ```ignore
/// tex::set_options(tex::TEXTURE_2D | tex::FLIP);
/// ```
///
/// But that does not guarantee you that you won't use values outside the enum set for these flags.
/// For example you could do:
///
/// ```ignore
/// set_options(4 | 5);
/// ```
///
/// Which is bad for obvious reasons. Now imagine you had an enum to hold all of those flags and a
/// common type that does not accept any types other than the enum variants and itself. This is exactly what
/// `bitmask!` does for you! It generates an enum with the variants (flags) you supply and a struct that
/// holds a mask composed of arbitrary amount of these variants. So now our example would look like this:
///
/// ```
/// # mod tex {
/// #     pub const TEXTURE_2D: u32   = 1;
/// #     pub const TEXTURE_3D: u32   = 2;
/// #     pub const FLIP: u32         = 4;
/// #     pub fn set_options(mask: u32) {}
/// # }
/// # #[macro_use] extern crate bitmask; fn main() {
/// bitmask! {
///     pub mask TexMask: u32 where flags TexOption {
///         Texture2d = tex::TEXTURE_2D,
///         Texture3d = tex::TEXTURE_3D,
///         Flip = tex::FLIP
///     }
/// }
///
/// fn set_options(mask: TexMask) {
///     tex::set_options(*mask);
/// }
///
/// set_options(TexOption::Texture2d | TexOption::Flip);
/// # }
/// ```
///
/// # Things that can change with time but are doable:
///
/// If for some reason you want to define the enum and the struct yourself you can do so and use the
/// `@IMPL` branch of the macro to implement the methods. The only restrictions are that your
/// struct's inner field must be named `mask` and the enum should have the same size as the struct
/// which can be achieved through the `#[repr()]` modifier with the same integer type as the field `mask`.
///
/// You can also implement `Into<struct_name>` and `Deref` for your own custom type if you want to
/// use it with the preimplemented methods for the mask but does not apply for the trait impls
/// like `BitOr` for example and as stated **this can change** with time.
///
/// # Examples:
///
/// ```
/// # #[macro_use] extern crate bitmask; fn main() {
/// bitmask! {
///     mask BitMask: u32 where flags Flags {
///         Flag1       = 0x00000001,
///         Flag2       = 0x000000F0,
///         Flag3       = 0x00000800,
///         Flag123     = 0x000008F1,
///         // Note that function calls like `isize::min_value()`
///         // can't be used for enum discriminants in Rust.
///         FlagMax     = ::std::u32::MAX
///     }
/// }
///
/// // You can add meta attributes like documentation (`#[doc = ""]`) to each element of the macro.
/// bitmask! {
///     /// Doc comment for the struct
///     pub mask SomeOtherMask: isize where
///     /// Doc comment for the enum
///     flags SomeOtherFlags {
///         /// Doc comment for the flag
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
/// Maybe not the best example but still... Cake is love!
///
/// ```
/// # #[macro_use] extern crate bitmask; fn main() {
/// bitmask! {
///     mask Cake: u8 where flags Ingredients {
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
// TODO: simplify the parsing when https://github.com/rust-lang/rust/issues/24189 is resolved
#[macro_export]
macro_rules! bitmask {
    // First stage (Struct)
    //
    // Parse struct meta attributes, its name and its type.
    (
        $(#[$st_attr: meta])* mask $st_name: ident : $T: tt where $($token: tt)+
    ) => {
        bitmask! {
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            $($token)+
        }
    };

    // Second stage (Enum)
    //
    // Parse enum meta attributes and its name.
    (
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        $(#[$en_attr: meta])* flags $en_name: ident {  $($token: tt)+ }
    ) => {
        bitmask! {
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                []
            ],
            $($token)+
        }
    };

    // Third stage (Flag meta)
    //
    // Parse flag meta attributes.
    (
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$prev_attr: meta])* ]
        ],
        #[$next_attr: meta] $($token: tt)*
    ) => {
        bitmask! {
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                [ $(#[$prev_attr])* #[$next_attr] ]
            ],
            $($token)*
        }
    };

    // Fourth stage (Flag name and value)
    //
    // Parse the flag itself.
    // Handles the case with trailing comma.
    (
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$next_attr: meta])* ]
        ],
        $next_name: ident = $next_value: expr, $($token: tt)*
    ) => {
        bitmask! {
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                meta: [ $(#[$next_attr])* ]
                flag: $next_name = $next_value;
                []
            ],
            $($token)*
        }
    };

    // Fifth stage (Missing trailing comma)
    //
    // Parse the last flag if missing trailing comma.
    (
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$next_attr: meta])* ]
        ],
        $next_name: ident = $next_value: expr
    ) => {
        bitmask! {
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                meta: [ $(#[$next_attr])* ]
                flag: $next_name = $next_value;
                []
            ],
        }
    };

    // Sixth stage (EOL)
    //
    // End of the line. Time to declare the struct and enum.
    (
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )+
            []
        ],
    ) => {
        #[repr($T)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        #[allow(dead_code)]
        $(#[$en_attr])*
        enum $en_name {
            $(
                $(#[$flag_attr])*
                $flag_name = $flag_value
            ),+
        }
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        #[allow(dead_code)]
        $(#[$st_attr])*
        struct $st_name {
            mask: $T
        }
        bitmask!(@IMPL $st_name $T $en_name, {
            $($flag_name = $flag_value),+
        });
    };

    //========================================= PUB API ==========================================//

    // First pub stage (Struct)
    //
    // Parse struct meta attributes, its name and its type.
    (
        $(#[$st_attr: meta])* pub mask $st_name: ident : $T: tt where $($token: tt)+
    ) => {
        bitmask! {
            pub
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            $($token)+
        }
    };

    // Second pub stage (Enum)
    //
    // Parse enum meta attributes and its name.
    (
        pub
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        $(#[$en_attr: meta])* flags $en_name: ident {  $($token: tt)+ }
    ) => {
        bitmask! {
            pub
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                []
            ],
            $($token)+
        }
    };

    // Third pub stage (Flag meta)
    //
    // Parse flag meta attributes.
    (
        pub
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$prev_attr: meta])* ]
        ],
        #[$next_attr: meta] $($token: tt)*
    ) => {
        bitmask! {
            pub
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                [ $(#[$prev_attr])* #[$next_attr] ]
            ],
            $($token)*
        }
    };

    // Fourth pub stage (Flag name and value)
    //
    // Parse the flag itself.
    // Handles the case with trailing comma.
    (
        pub
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$next_attr: meta])* ]
        ],
        $next_name: ident = $next_value: expr, $($token: tt)*
    ) => {
        bitmask! {
            pub
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                meta: [ $(#[$next_attr])* ]
                flag: $next_name = $next_value;
                []
            ],
            $($token)*
        }
    };

    // Fifth pub stage (Missing trailing comma)
    //
    // Parse the last flag if missing trailing comma.
    (
        pub
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )*
            [ $(#[$next_attr: meta])* ]
        ],
        $next_name: ident = $next_value: expr
    ) => {
        bitmask! {
            pub
            st_meta: [ $(#[$st_attr])* ],
            st_name: $st_name,
            mask_type: $T,
            en_meta: [ $(#[$en_attr])* ],
            en_name: $en_name,
            flags: [
                $(
                    meta: [ $(#[$flag_attr])* ]
                    flag: $flag_name = $flag_value;
                )*
                meta: [ $(#[$next_attr])* ]
                flag: $next_name = $next_value;
                []
            ],
        }
    };

    // Sixth pub stage (EOL)
    //
    // End of the line. Time to declare the struct and enum.
    (
        pub
        st_meta: [ $(#[$st_attr: meta])* ],
        st_name: $st_name: ident,
        mask_type: $T: tt,
        en_meta: [ $(#[$en_attr: meta])* ],
        en_name: $en_name: ident,
        flags: [
            $(
                meta: [ $(#[$flag_attr: meta])* ]
                flag: $flag_name: ident = $flag_value: expr;
            )+
            []
        ],
    ) => {
        #[repr($T)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        #[allow(dead_code)]
        $(#[$en_attr])*
        pub enum $en_name {
            $(
                $(#[$flag_attr])*
                $flag_name = $flag_value
            ),+
        }
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        #[allow(dead_code)]
        $(#[$st_attr])*
        pub struct $st_name {
            mask: $T
        }
        bitmask!(@IMPL $st_name $T $en_name, {
            $($flag_name = $flag_value),+
        });
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
        /// Doc comment
        mask BitMask: isize where
        /// Doc comment
        flags Flags {
            Flag1      = 0b00000001,
            /// Doc comment
            Flag2      = 0b00000010,
            Flag3      = 0b00000100,
            FlagMin    = ::std::isize::MIN,
            /// Doc comment
            Flag123    = 0b00000111
        }
    }
    bitmask! {
        mask A: u8 where flags B {
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
                pub mask InnerMask: u8 where flags InnerFlags {
                    InnerFlag1 = 0
                }
            }
        }
        let _ = inner::InnerMask::new();
        let _ = inner::InnerFlags::InnerFlag1;
    }
}
