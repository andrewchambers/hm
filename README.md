# hm

A prototype and WIP and toy compiler for a tiny C like language that
adds a few features and removes most others.

Ideas:

- Type inference.
- Out of order definitions.
- C like semantics.

## Example code

```

extern printf(s: int, ...) -> int;

fn main (argc: int, argv: **char) -> int {
  let s = argv[0];
  printf("hello from %s!", s);
  0
}

```
