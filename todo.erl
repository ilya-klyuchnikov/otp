-module(todo).

-compile(export_all).

%% import
test1() ->
  &s{}.

%% initialization
test2() ->
  &s:s{a=1}.

%% import
test3() ->
  &s{a=1}.

%% access
test4(X) ->
  X&s:s.a.

%% import
test5(X) ->
  X&s.a.

% illegal guard expression
test6(X) when X&s:s.a ->
  true.

% illegal guard expression
test7(X) when X /= &s:s{} ->
  true.

% update
test8(X) ->
  X&s:a{a = 1}.

% import + update
test9(X) ->
  X&s:a{a = 1}.