# bitmask

A bitmask generator for enum scoped bit flags

[Documentation](https://docs.rs/bitmask)

## Usage

Add `bitmask` as a dependency in your `Cargo.toml`:

```toml
[dependency]
bitmask = "^0.5.0"
```

Then add this snippet to your crate's root:

```rust
#[macro_use]
extern crate bitmask;
```

## Features

Bitmask supports one feature: `std`. This is enabled by default, and will draw
in the standard library and also automatically derive of `Hash` and `Debug` for
generated types. If you prefer not to derive these features, then ensure you
do not enable the default features.

## Examples

Run a specific example with `cargo run --example <name>`.

## Similar crates

* [bitflags!](https://crates.io/crates/bitflags)
