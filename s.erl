-module(s).

-export([main/0]).

main() ->
  init_definitions(),
  test1(),
  halt(0).

init_definitions() ->
  struct_prototype:define(a,a,
    {{a1,a1_default},
      {a2,a2_default}}
  ),
  struct_prototype:define(a,b,
    {{b1,b1_default},
      {b2,b2_default}}
  ).

tag(&a:a{}) ->
  a;
tag(&a:b{}) ->
  b.

test1() ->
  Str = &a:a{},
  Tag = tag(Str),
  a = Tag,
  ok.
