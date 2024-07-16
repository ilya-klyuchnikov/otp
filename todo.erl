-module(todo).

-compile(export_all).

%% import
test1() ->
  &s{}.


%% import
test3() ->
  &s{a=1}.

%% import
test5(X) ->
  X&s.a.

% illegal guard expression
test6(X) when X&s:s.a ->
  true.

% illegal guard expression
test7(X) when X /= &s:s{} ->
  true.

% import + update
test9(X) ->
  X&a{a = 1}.
