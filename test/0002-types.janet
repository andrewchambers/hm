
type foo = int;

type bar = *int;

type baz = struct {};

type bang = struct {
  foo : int;
  bar : *int;
  baz : struct {};
  pself : *bang;
};