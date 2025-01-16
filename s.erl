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
  test11(),
  test12(),
  test13(),
  test14(),
  test15(),
  test16(),
  test17(),
  test18(),
  test19(),
  test20(),
  ok.

init_definitions() ->
  struct_prototype:define(a,a,
    {{a1,a1_default},
      {a2,a2_default}}
  ),
  struct_prototype:define(a,b,
    {{b1,b1_default},
      {b2,b2_default}}
  ),
  struct_prototype:define(a,c,
    {{a1,a1_default},
      {a2,a2_default}}
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

test12() ->
  Str1 = &a:a{a1 = 1},
  Str2 = &a:a{},
  a1 = is_aa1(Str1),
  a1 = is_aa12(Str1),
  no = is_aa1(Str2),
  no = is_aa12(Str2),
  no = is_aa1([]),
  no = is_aa12([]).

test13() ->
  Str1 = &a:a{a1 = 1},
  Str2 = &a:c{a1 = 1},
  a1 = is_aa1(Str1),
  no = is_aa1(Str2).

test14() ->
  Str1 = &a:a{a1 = 1},
  Str2 = &a:c{a1 = 1},
  yes = is_a_call(Str1),
  no = is_a_call(Str2).

test15() ->
  % testing complex X&a:a.field in guards
  A = &a:a{a1 = a},
  B = &a:b{b1 = a},
  C = &a:c{},
  F1 =  fun (X) when X&a:a.a1 == a; X&a:b.b1 == a -> true; (_) -> false end,
  true = F1(A),
  true = F1(B),
  false = F1(C),
  F2 =  fun (X) when X&a:a.a1 == a -> a; (X) when X&a:b.b1 == a -> b; (_) -> other end,
  a = F2(A),
  b = F2(B),
  other = F2(C).

test16() ->
  A = &a:a{a1 = a},
  B = &a:b{b1 = a},
  C = &a:c{},
  ABC = [A, B, C],
  [A] = [X || X <- ABC, X&a:a.a1 == a].

test17() ->
  A1 = &a:a{a1 = a},
  A2 = &a:a{a1 = b},
  AA = [A1, A2],
  [A1] = [X || X <- AA, id(X&a:a.a1 == a)].

test18() ->
  A = &a:a{a1 = a},
  B = &a:b{},
  AB = [A, B],
  fail = try
    % fails with badstruct
    [A] = [X || X <- AB, id(X&a:a.a1 == a)]
  catch _:_ -> fail end.

test19() ->
  A = &a:a{a1 = a},
  {ok, a} = get_any_a1(A),
  no = get_any_a1({}),
  a = get_any_a2(A),
  a = get_any_a3(A).

test20() ->
  A = &a:a{},
  A1 = A&{a1 = a1, a2 = a2},
  &{a1 = a1} = A1.

match1(&a:a{a1 = a1}, &a:b{b1 = b1}) ->
  f1;
match1(&a:a{a2 = a2}, &a:b{b2 = b2}) ->
  f2;
match1(#{a1 := &a:a{a1 = &a:b{b2 = #{a1 := &a:a{a1 = A1}}}}}, _) ->
  A1;
match1(_, _) ->
  default.

id(X) -> X.

is_aa1(X) when X&a:a.a1 == 1 ->
  a1;
is_aa1(_) ->
  no.

is_aa12(X) when X&.a1 == 1 ->
  a1;
is_aa12(_) ->
  no.

is_a_call(X) when struct_name(X) == a ->
  yes;
is_a_call(_) ->
  no.

get_any_a1(&{a1 = A}) -> {ok, A};
get_any_a1(_) -> no.

get_any_a2(S) ->
  S&.a1.

get_any_a3(S) ->
  S&.a1.
