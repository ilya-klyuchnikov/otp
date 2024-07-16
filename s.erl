-module(s).

-export([main/0, tests/0, id/1]).

main() ->
  tests(),
  halt(0).

tests() ->
  init_definitions(),
  test1(),
  test2(),
  test2i(),
  test2i_fail(),
  test2f(),
  test3(),
  test4(),
  test6(),
  test6i(),
  test7(),
  test8(),
  test9(),
  test10(),
  test11().

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

get_a12(&a:a{a3 = A3}) ->
  A3;
get_a12(&a:a{a1 = A1}) ->
  A1.

get_a13(&a:a{a1 = &a:a{}}) ->
  a1;
get_a13(&a:a{a1 = A1}) ->
  A1.


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

test2i() ->
  Str = &a:a{a1 = non_default},
  A1 = get_a1(Str),
  non_default = A1,
  ok.

test2i_fail() ->
  Res =
  try
    _Str = &a:a{bad_field = non_default},
    no_fail
  catch
    _:_ -> fail
  end,
  fail = Res.

test2f() ->
  Str = &a:a{},
  A1 = Str&a:a.a1,
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

test6i() ->
  Str = &a:a{a1 = s:id(a1), a2 = s:id(a2)},
  As = get_as(Str),
  {a1, a2} = As,
  ok.


test7() ->
  Str = &a:a{},
  A1 = get_a12(Str),
  a1_default = A1,
  a1_default = get_a13(Str),
  ok.

%% updates
test8() ->
  Str = &a:a{a1 = a10, a2 = a20},
  Str1 = Str&a:a{a1 = a11, a2 = a21},
  a11 = Str1&a:a.a1,
  a21 = Str1&a:a.a2,
  ok.

%% maps/hashes
test9() ->
  Str1 = &a:a{},
  Str2 = &a:b{},
  M = #{Str1 => a, Str2 => b},
  a = maps:get(Str1, M),
  b = maps:get(Str2, M),
  ok.

test10() ->
  Str1 = &a:a{},
  Str2 = &a:b{},
  M = #{&a:a{} => a, &a:b{} => b, &a:a{a1 = a1, a2 = a2} => a1, &a:b{b1 = b1, b2 = b2} => b1},
  a = maps:get(Str1, M),
  b = maps:get(Str2, M),
  a1 = maps:get(&a:a{a1 = a1, a2 = a2}, M),
  b1 = maps:get(&a:b{b1 = b1, b2 = b2}, M),
  ok.

test11() ->
  default = match1(id(1), id(2)),
  f1 = match1(id(&a:a{a1 = a1, a2 = a3}), &a:b{b1 = b1}),
  f2 = match1(id(&a:a{a1 = a2, a2 = a2}), &a:b{b1 = b1, b2 = b2}),
  42 = match1(id(#{a1 => &a:a{a1 = &a:b{b2 = #{a1 => &a:a{a1 = 42}}}}}), ignore),
  ok.

match1(&a:a{a1 = a1}, &a:b{b1 = b1}) ->
  f1;
match1(&a:a{a2 = a2}, &a:b{b2 = b2}) ->
  f2;
match1(#{a1 := &a:a{a1 = &a:b{b2 = #{a1 := &a:a{a1 = A1}}}}}, _) ->
  A1;
match1(_, _) ->
  default.

id(X) -> X.
