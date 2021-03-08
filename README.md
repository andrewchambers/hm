# hm

A prototype/wip/toy compiler for a tiny C like language that
adds a few features and removes most others.

Inspired by:

- https://bellard.org/otcc
- https://github.com/rswier/c4
- https://github.com/rui314/chibicc

Initial plan:

- C like semantics (compile to C perhaps?).
- Function local type inference.
- Spartan simplicity.

Future ideas:

- Self hosting.
- Owned values.
- C compatible generics.

## Example code

```

extern printf(s: int, ...) -> int;

fn main (argc: int, argv: **char) -> int {
  let s = argv[0];
  printf("hello from %s!", s);
  0
}

```


## C compatible generics

The key idea is that generic types are allowed 
in exported functions provided they are accessed via void* in C code.

Consider this generic definition of malloc:

```

fn malloc(n: size = sizeof('t)) -> *'t

```

Now we can call malloc in some simple ways:

```

let p1 : *[32; int] = malloc(_);
let p2 : *void = malloc(1024);

```

This declaration of malloc is C ABI compatible and also type safe. 

There is an open question about how useful this will
be in practice, but it's fun to consider.