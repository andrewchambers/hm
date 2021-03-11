

# Types

## Primitive basic

- `void`
- `bool`

## Primitive numeric

- `i{8,16,32,64}`
- `u{8,16,32,64}`
- `size`
- `usize`

## Compound

- `*T` (pointer)
- `[N; T]` (sized array)
- `[*; T]` (unsized array)
- `struct { name: T, ... }`
- `owned T`

# Expressions

Types are inferred from context.
There is no operator precedence, parens used to disambiguate mixed infix ops.

## Lvalues

- `ident`
- `e[e]`
- `*e`

## Type operations

- `e : T` (type unification)
- `e as T` (numeric and pointer cast)
- `sizeof(T)` (type size)

## Untyped Literals

- `[0-9]+`
- `"..."`
- `'c'`

## Binops

- +
- -
- *
- /
- %
- &
- |
- ^
- >>
- <<

## Compare ops

- <
- >
- >=
- <=
- !=
- ==

## Short circuit

- && 
- ||

## Assignment

- =

## If 

- if e { e } else { e } 

## Blocks

- { s; ...; s }

## Block statement expressions

- let name;
- let name = expression;
- let name, ... = expression;
- expression 
- -> (return operator) 

## Ownership expressions

Only parameters, local variables and return values may be owned.

- `ident = take e` (convert non owned type to an owned type).
- `sink ident` (convert owned type to non owned and mark ident unusable).
- `sink expr` (convert owned type to non owned forgetting value).
- `lend ident` (convert owned type to non owned leaving ident intact).

It is an error to use an owned value without a sink expression or
assigning it to an ident.

It is an error to use a variable of an owned type without sinking it.

examples:
```
let x  = take 0:int;
sink x;
```

```
extern fn malloc() -> owned *[]byte;
extern fn free(owned *[]byte)
...
let p = malloc(1024);
free(p);
```

```
extern fn malloc() -> owned *[]byte;
extern fn free(owned *[]byte)
...
let p = malloc(1024);
free(p);
```

```
extern fn open(path: *[]byte) -> owned int;
extern fn read(fd: int, buf :*[]byte, sz: size);
extern fn close(fd: owned int);
...
let f = open("hello.txt");
let p = malloc(1024);
read(lend f, lend p, 1024);
close(f);
free(p);
```


