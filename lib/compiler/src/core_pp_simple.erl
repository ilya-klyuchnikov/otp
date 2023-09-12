-module(core_pp_simple).

-export([format/1]).

-spec format(Core :: any()) -> any().
format(Core) ->
  io_lib:format("~P", [Core, 100]).
