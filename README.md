# hm

A prototype/wip/toy compiler for a tiny C like language that
adds a few features and removes most others.

Initial plan:

- C like semantics, compile to C initially.
- Function local type inference.
- Simplicity.

## Example code

```

puts(s: *char) -> void;

fn main (argc: int, argv: **char) -> int {
  puts("hello!");
  0
}

```

