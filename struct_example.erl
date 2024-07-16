-module(struct_example).

-export([main/0]).

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
    halt(0).
