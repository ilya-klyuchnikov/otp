-module(struct_example).

-export([main/0, main1/0]).

% ./bin/erl -noshell -eval 'struct_example:main()'

main() ->
    struct_prototype:define(a,b,{{field1,1},{field2,2}}),
    Obj = struct_prototype:create(a,b,2),
    % Field1Prior = struct_prototype:read(Obj, field1),
    % Obj2 = struct_prototype:update(Obj, field1, blurf),
    % Obj3 = struct_prototype:update(Obj2, field1, 1),
    % {Obj =:= Obj3, Obj2, Obj3}.
    Test1 =
        if
            is_struct(Obj) -> struct;
            true -> other
        end,
    Test2 =
        case Obj of
            O when is_struct(O) -> struct;
            _ -> other
        end,

    erlang:display(io_lib_format:fwrite("~p_~p", [Test1, Test2])),
    erlang:display(io_lib_format:fwrite("~p", [Obj])),
    erlang:display({struct_prototype:module(Obj), struct_prototype:name(Obj)}),
    X = term_to_binary(Obj),
    erlang:display(X),
    Y = binary_to_term(X),
    erlang:display(Y),
    erlang:display(is_struct(Y)),
    erlang:display({struct_prototype:module(Y), struct_prototype:name(Y)}),
    halt(0).

main1() ->
    SerializedStruct = <<131,104,3,104,5,110,5,0,176,18,22,80,1,119,6,102,105,101,108,100,49,97,1,119,6,102,105,101,108,100,50,97,2,97,1,97,2>>,
    Struct = binary_to_term(SerializedStruct),
    erlang:display(Struct),
    erlang:display({struct_prototype:module(Struct), struct_prototype:name(Struct)}),
    halt(0).