# hm

A prototype/wip/toy compiler for a tiny C like language that
adds a few features and removes most others.

Inspired by:

- https://github.com/rswier/c4
- https://github.com/rui314/chibicc

Initial plan:

- C like semantics (compile to C perhaps?).
- Function local type inference.
- Spartan simplicity.

Future ideas:

- Self hosting.
- Owned values.

## Example code

```

extern printf(s: int, ...) -> int;

fn main (argc: int, argv: **char) -> int {
  let s = argv[0];
  printf("hello from %s!", s);
  0
}

```
