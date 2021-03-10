
# No operator precedence

Mixing infix operators requires parens just like pony.

# C compatible generics

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