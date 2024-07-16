-module(struct_example).

-export([main/0, main1/0, test1/0]).

% ./bin/erl -noshell -eval 'struct_example:main()'

main() ->
    struct_prototype:define(a,foo,{}),
    erlang:display(debug),
    struct_prototype:define(a,b,{{field1,default1},{field2,default2}}),
    Obj = struct_prototype:create(a,b),
    Obj01 = &a:b{},
%%    _Obj1 = struct_prototype:create(a,foo,1),
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
    erlang:display({test1, struct_prototype:read(Obj, field1), struct_prototype:read(Obj, field2)}),
    Obj1 = struct_prototype:update(Obj, field1, f1),
    Obj2 = struct_prototype:update(Obj1, field2, f2),
    erlang:display({test2, struct_prototype:read(Obj2, field1), struct_prototype:read(Obj2, field2)}),
    X = term_to_binary(Obj),
    erlang:display(X),
    Y = binary_to_term(X),
    erlang:display(Y),
    erlang:display(is_struct(Y)),
    erlang:display(debug),
    erlang:display(is_struct(Obj)),
    erlang:display(struct_prototype:is(Obj, a, b)),
    % fails with bad_arg - expectedly !!!
%%    erlang:display({struct_prototype:module(Y), struct_prototype:name(Y)}),
    halt(0).

test1() ->
    &a:b{}.

main1() ->
    SerializedStruct = <<131,104,3,104,5,110,5,0,176,18,22,80,1,119,6,102,105,101,108,100,49,97,1,119,6,102,105,101,108,100,50,97,2,97,1,97,2>>,
    Struct = binary_to_term(SerializedStruct),
    erlang:display(Struct),
    erlang:display({struct_prototype:module(Struct), struct_prototype:name(Struct)}),
    halt(0).