-module(s).

-export([main/0, tests/0]).

main() ->
  tests(),
  halt(0).

tests() ->
  init_definitions(),
  test1(),
  test2(),
  test3(),
  test4(),
  test6().

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

get_a1(&a:a{a1 = A1}) ->
  A1.

get_as(&a:a{a1 = A1, a2 = A2}) ->
  {A1, A2}.

test1() ->
  Str = &a:a{},
  Tag = tag(Str),
  a = Tag,
  ok.

test2() ->
  Str = &a:a{},
  A1 = get_a1(Str),
  a1_default = A1,
  ok.

test3() ->
  Str = &a:a{},
  &a:a{} = Str,
  ok.

test4() ->
  Str = &a:a{},
  &a:a{a1 = A1} = Str,
  a1_default = A1,
  ok.

%% bad_match
test5() ->
  Str = &a:a{},
  &a:a{b1 = A1} = Str,
  a1_default = A1,
  ok.

test6() ->
  Str = &a:a{},
  As = get_as(Str),
  {a1_default, a2_default} = As,
  ok.
