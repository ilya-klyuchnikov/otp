%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(erts_debug).
-moduledoc false.

%% Low-level debugging support. EXPERIMENTAL!

-export([size/1,df/1,df/2,df/3,dis_to_file/2,ic/1]).

%% This module contains the following *experimental* BIFs:
%%   disassemble/1
%%   breakpoint/2
%%   same/2
%%   flat_size/1

%%% BIFs

-export([breakpoint/2, disassemble/1, dist_ext_to_term/2,
         flat_size/1, get_internal_state/1, instructions/0,
         interpreter_size/0,
         map_info/1, same/2, set_internal_state/2,
         size_shared/1, copy_shared/1, copy_shared/2,
         dirty_cpu/2, dirty_io/2, dirty/3,
         lcnt_control/1, lcnt_control/2, lcnt_collect/0, lcnt_clear/0,
         lc_graph/0, lc_graph_to_dot/2,
         lc_graph_merge/0, lc_graph_merge/1, lc_graph_merge/2,
         alloc_blocks_size/1]).

%% Reroutes calls to the given MFA to error_handler:breakpoint/3
%%
%% Note that this is potentially unsafe as compiled code may assume that the
%% targeted function returns a specific type, triggering undefined behavior if
%% this function were to return something else.
%%
%% For reference, the debugger avoids the issue by purging the affected module
%% and interpreting all functions in the module, ensuring that no assumptions
%% are made with regard to return or argument types.
-spec breakpoint(MFA, Flag) -> non_neg_integer() when
      MFA :: {Module :: module(),
              Function :: atom(),
              Arity :: arity() | '_'},
      Flag :: boolean().

breakpoint(_, _) ->
    erlang:nif_error(undef).

-spec disassemble(What) -> false | undef | Result when
      What :: MFA | Address,
      Result :: {Address, Code, MFA},
      MFA :: mfa(),
      Address :: non_neg_integer(),
      Code :: binary().

disassemble(_) ->
    erlang:nif_error(undef).

-spec dist_ext_to_term(Tuple, Binary) -> term() when
      Tuple :: tuple(),
      Binary :: binary().

dist_ext_to_term(_, _) ->
    erlang:nif_error(undef).

-spec flat_size(Term) -> non_neg_integer() when
      Term :: term().

flat_size(_) ->
    erlang:nif_error(undef).

-spec size_shared(Term) -> non_neg_integer() when
      Term :: term().

size_shared(_) ->
    erlang:nif_error(undef).

-spec copy_shared(Term) -> term() when
      Term :: term().

copy_shared(Term) ->
    copy_shared(Term, false).

-spec copy_shared(Term, CopyLiterals) -> term() when
      Term :: term(),
      CopyLiterals :: true | false.

copy_shared(_, _) ->
    erlang:nif_error(undef).

-spec get_internal_state(W) -> term() when
      W :: reds_left | node_and_dist_references | monitoring_nodes
         | next_pid | 'DbTable_words' | check_io_debug | lc_graph
         | process_info_args | processes | processes_bif_info
         | max_atom_out_cache_index | nbalance | available_internal_state
         | force_heap_frags | memory
         | {process_status, pid()}
         | {link_list, pid() | port() | node()}
         | {monitor_list, pid() | node()}
         | {channel_number, non_neg_integer()}
         | {have_pending_exit, pid() | port() | atom()}
         | {binary_info, binary()}
         | {term_to_binary_no_funs, term()}
         | {dist_port, port()}
         | {atom_out_cache_index, atom()}
         | {fake_scheduler_bindings,
            default_bind | spread | processor_spread | thread_spread
            | thread_no_node_processor_spread | no_node_processor_spread
            | no_node_thread_spread | no_spread | unbound}
         | {reader_groups_map, non_neg_integer()}.

get_internal_state(_) ->
    erlang:nif_error(undef).

-spec instructions() -> [string()].

instructions() ->
    erlang:nif_error(undef).

-spec interpreter_size() -> pos_integer().

interpreter_size() ->
    erlang:nif_error(undef).

-spec ic(F) -> Result when
      F :: function(),
      Result :: term().

ic(F) when is_function(F) ->
    Is0 = erlang:system_info(instruction_counts),
    R   = F(),
    Is1 = erlang:system_info(instruction_counts),
    Is  = lists:keysort(2,[{I,C1 - C0}||{{I,C1},{I,C0}} <- lists:zip(Is1,Is0)]),
    _   = [io:format("~12w ~w~n", [C,I])||{I,C}<-Is],
    io:format("Total: ~w~n",[lists:sum([C||{_I,C}<-Is])]),
    R.

-spec lcnt_control
    (copy_save, boolean()) -> ok;
    (mask, list(atom())) -> ok.

lcnt_control(_Option, _Value) ->
    erlang:nif_error(undef).

-spec lcnt_control
    (copy_save) -> boolean();
    (mask) -> list(atom()).

lcnt_control(_Option) ->
    erlang:nif_error(undef).

-type lcnt_lock_info() :: {atom(), term(), atom(), term()}.

-spec lcnt_collect() ->
    list({duration, {non_neg_integer(), non_neg_integer()}} |
         {locks, list(lcnt_lock_info())}).

lcnt_collect() ->
    erlang:nif_error(undef).

-spec lcnt_clear() -> ok.
lcnt_clear() ->
    erlang:nif_error(undef).

-spec same(Term1, Term2) -> boolean() when
      Term1 :: term(),
      Term2 :: term().

same(_, _) ->
    erlang:nif_error(undef).

-spec set_internal_state(available_internal_state, boolean()) -> boolean();
                           (reds_left, non_neg_integer()) -> true;
                           (block, non_neg_integer()) -> true;
                           (sleep, non_neg_integer()) -> true;
                           (block_scheduler, non_neg_integer()) -> true;
                           (next_pid, non_neg_integer()) -> false | integer();
                           (force_gc, pid() | atom()) -> boolean();
                           (send_fake_exit_signal, {pid() | port(), pid(), term()}) -> dead | message | unaffected | exit;
                           (colliding_names, {atom(), non_neg_integer()}) ->
                                   [atom()];
                           (binary_loop_limit, default) -> -1;
                           (binary_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (re_loop_limit, default) -> -1;
                           (re_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (unicode_loop_limit, default) -> -1;
                           (unicode_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (test_long_gc_sleep, non_neg_integer()) -> true;
                           (kill_dist_connection, port()) -> boolean();
                           (not_running_optimization, boolean()) -> boolean();
                           (wait, deallocations) -> ok.

set_internal_state(_, _) ->
    erlang:nif_error(undef).

-spec dirty_cpu(Term1, Term2) -> term() when
      Term1 :: term(),
      Term2 :: term().

dirty_cpu(_, _) ->
    erlang:nif_error(undef).

-spec dirty_io(Term1, Term2) -> term() when
      Term1 :: term(),
      Term2 :: term().

dirty_io(_, _) ->
    erlang:nif_error(undef).

-spec dirty(Term1, Term2, Term3) -> term() when
      Term1 :: term(),
      Term2 :: term(),
      Term3 :: term().

dirty(_, _, _) ->
    erlang:nif_error(undef).

%%% End of BIFs

%% size(Term)
%%  Returns the size of Term in actual heap words. Shared subterms are
%%  counted once.  Example: If A = [a,b], B =[A,A] then size(B) returns 8,
%%  while flat_size(B) returns 12.

-spec size(term()) -> non_neg_integer().

-record(s, {seen, maps}).

size(Term) ->
    {Sum,_} = size(Term, #s{seen=gb_trees:empty(),maps=[]}, 0),
    Sum.

size([H|T]=Term, Seen0, Sum0) ->
    case remember_term(Term, Seen0) of
	seen -> {Sum0,Seen0};
	Seen1 ->
	    {Sum,Seen} = size(H, Seen1, Sum0+2),
	    size(T, Seen, Sum)
    end;
size({}, Seen0, Sum0) ->
    %% Tuples of size 0 all points to a constant literal so we count
    %% them as size zero
    {Sum0,Seen0};
size(Tuple, Seen0, Sum0) when is_tuple(Tuple) ->
    case remember_term(Tuple, Seen0) of
	seen -> {Sum0,Seen0};
	Seen ->
	    Sum = Sum0 + 1 + tuple_size(Tuple),
	    tuple_size(1, tuple_size(Tuple), Tuple, Seen, Sum)
    end;
size(Map, Seen0, Sum) when is_map(Map) ->
    case remember_term(Map, Seen0) of
	seen -> {Sum,Seen0};
	Seen -> map_size(Map, Seen, Sum)
    end;
size(Fun, Seen0, Sum) when is_function(Fun) ->
    case remember_term(Fun, Seen0) of
	seen -> {Sum,Seen0};
	Seen -> fun_size(Fun, Seen, Sum)
    end;
size(Term, Seen0, Sum) ->
    case erts_debug:flat_size(Term) of
	0 -> {Sum,Seen0};
	Sz ->
	    case remember_term(Term, Seen0) of
		seen -> {Sum,Seen0};
		Seen -> {Sum+Sz,Seen}
	    end
    end.

tuple_size(I, Sz, _, Seen, Sum) when I > Sz ->
    {Sum,Seen};
tuple_size(I, Sz, Tuple, Seen0, Sum0) ->
    {Sum,Seen} = size(element(I, Tuple), Seen0, Sum0),
    tuple_size(I+1, Sz, Tuple, Seen, Sum).

map_size(Map,Seen0,Sum0) ->
    %% Danger:
    %% The internal nodes from erts_internal:map_hashmap_children/1
    %% is not allowed to leak anywhere. They are only allowed in
    %% containers (cons cells and tuples, not maps), in gc and
    %% in erts_debug:same/2
    case erts_internal:term_type(Map) of
        flatmap ->
            Kt = erts_internal:map_to_tuple_keys(Map),
            Vs = maps:values(Map),
            {Sum1,Seen1} = size(Kt,Seen0,Sum0),
            fold_size(Vs,Seen1,Sum1+length(Vs)+3);
        hashmap ->
            Cs = erts_internal:map_hashmap_children(Map),
            fold_size(Cs,Seen0,Sum0+length(Cs)+2);
        hashmap_node ->
            Cs = erts_internal:map_hashmap_children(Map),
            fold_size(Cs,Seen0,Sum0+length(Cs)+1)
    end.

fun_size(Fun, Seen, Sum) ->
    case erlang:fun_info(Fun, type) of
	{type,external} ->
	    {Sum + erts_debug:flat_size(Fun),Seen};
	{type,local} ->
	    Sz = erts_debug:flat_size(fun() -> ok end),
	    {env,Env} = erlang:fun_info(Fun, env),
	    fold_size(Env, Seen, Sum+Sz+length(Env))
    end.

fold_size([H|T], Seen0, Sum0) ->
    {Sum,Seen} = size(H, Seen0, Sum0),
    fold_size(T, Seen, Sum);
fold_size([], Seen, Sum) -> {Sum,Seen}.

remember_term(Term, #s{maps=Ms}=S) when is_map(Term) ->
    case is_term_seen(Term, Ms) of
        false -> S#s{maps=[Term|Ms]};
        true  -> seen
    end;
remember_term(Term, #s{seen=T}=S) ->
    case gb_trees:lookup(Term,T) of
	none -> S#s{seen=gb_trees:insert(Term,[Term],T)};
	{value,Terms} ->
	    case is_term_seen(Term, Terms) of
		false -> S#s{seen=gb_trees:update(Term,[Term|Terms],T)};
		true  -> seen
	    end
    end.

-spec is_term_seen(term(), [term()]) -> boolean().

is_term_seen(Term, [H|T]) ->
    case erts_debug:same(Term, H) of
	true -> true;
	false -> is_term_seen(Term, T)
    end;
is_term_seen(_, []) -> false.

%% df(Mod)              -- Disassemble Mod to file Mod.dis.
%% df(Mod, Func)        -- Disassemble Mod:Func/Any to file Mod_Func.dis.
%% df(Mod, Func, Arity) -- Disassemble Mod:Func/Arity to file Mod_Func_Arity.dis.

-type df_ret() :: 'ok' | {'error', {'badopen', module()}} | {'undef', module()}.

-spec df(module()) -> df_ret().

df(Mod) when is_atom(Mod) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, ".dis"]),
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec df(module(), atom()) -> df_ret().

df(Mod, Func) when is_atom(Mod), is_atom(Func) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, "_", Func, ".dis"]),
	    Fs = [{Mod,Func1,Arity} || {Func1,Arity} <- Fs0, Func1 =:= Func],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec df(module(), atom(), arity()) -> df_ret().

df(Mod, Func, Arity) when is_atom(Mod), is_atom(Func) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, "_", Func, "_", Arity, ".dis"]),
	    Fs = [{Mod,Func1,Arity1} || {Func1,Arity1} <- Fs0,
					Func1 =:= Func, Arity1 =:= Arity],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec dis_to_file(module(), file:filename()) -> df_ret().

dis_to_file(Mod, Name) when is_atom(Mod) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

dff(Name, Fs) ->
    case file:open(Name, [write,raw,delayed_write]) of
	{ok,F} ->
	    try
		dff_1(F, Fs)
	    after
		_ = file:close(F)
	    end;
	{error,Reason} ->
	    {error,{badopen,Reason}}
    end.

dff_1(File, Fs) ->
    lists:foreach(fun(Mfa) ->
                          disassemble_function(File, Mfa),
                          file:write(File, "\n")
                  end, Fs).

disassemble_function(File, {_,_,_}=MFA) ->
    cont_dis(File, erts_debug:disassemble(MFA), MFA).

cont_dis(_, false, _) -> ok;
cont_dis(File, {Addr,Str,MFA}, MFA) ->
    ok = file:write(File, Str),
    cont_dis(File, erts_debug:disassemble(Addr), MFA);
cont_dis(_, {_,_,_}, _) -> ok.

-spec map_info(Map) -> list() when
      Map :: map().

map_info(_) ->
    erlang:nif_error(undef).

%% Create file "lc_graph.<pid>" with all actual lock dependencies
%% recorded so far by the VM.
%% Needs debug VM or --enable-lock-checking config, returns 'notsup' otherwise.
lc_graph() ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:get_internal_state(lc_graph).

%% Convert "lc_graph.<pid>" file to https://www.graphviz.org dot format.
lc_graph_to_dot(OutFile, InFile) ->
    LL0 = lcg_read_file(InFile),
    LL1 = lcg_simplify_graph(LL0),

    case file:open(OutFile, [exclusive]) of
        {ok, Out} ->
            ok = file:write(Out, "digraph G {\n"),

            [dot_print_lock(Out, Lck) || Lck <- LL1],

            ok = file:write(Out, "}\n"),
            ok = file:close(Out);

        {error,eexist} ->
            {"File already exists", OutFile}
    end.

dot_print_lock(Out, {Name, Direct, _Indirect}) ->
    [dot_print_edge(Out, From, Name) || From <- Direct],
    ok.

dot_print_edge(Out, From, To) ->
    io:format(Out, "~p -> ~p;\n", [From, To]).


%% Merge several "lc_graph" files into one file.
lc_graph_merge() ->
    lc_graph_merge("lc_graph.merged").

lc_graph_merge(OutFile) ->
    lc_graph_merge(OutFile, "lc_graph.*").

lc_graph_merge(OutFile, [C|_]=Wildcard) when is_integer(C) ->
    lc_graph_merge(OutFile, filelib:wildcard(Wildcard));
lc_graph_merge(OutFile, InFiles) ->
    LLs = [lcg_read_file(File) || File <- InFiles],

    Res = lists:foldl(fun(A, B) -> lcg_merge(A, B) end,
                      hd(LLs),
                      tl(LLs)),
    case file:open(OutFile, [exclusive]) of
        {ok, Out} ->
            try
                lcg_print(Out, Res)
            after
                file:close(Out)
            end,
            ok;
        {error, eexist} ->
            {"File already exists", OutFile}
    end.

lcg_read_file(File) ->
    {ok, [LL]} = file:consult(File),
    lcg_expand_lock_names(LL).

lcg_expand_lock_names([{"NO LOCK", 0} | LL]) ->
    Map = #{Id => Name || {Name, Id, _, _} <- LL},
    [begin
         Direct = [maps:get(From,Map) || From <- DirectIds, From =/= 0],
         Indirect = [maps:get(From,Map) || From <- IndirectIds, From =/= 0],

         {Name, Direct, Indirect}
     end
     || {Name, _Id, DirectIds, IndirectIds} <- LL];
lcg_expand_lock_names(LL) ->
    LL. % assume already expanded format

lcg_merge(A, B) ->
    lists:zipwith(fun(LA, LB) -> lcg_merge_locks(LA, LB) end,
                  A, B).

lcg_merge_locks(L, L) ->
    L;
lcg_merge_locks({Name, DA, IA}, {Name, DB, IB}) ->
    Direct = lists:umerge(lists:sort(DA), lists:sort(DB)),
    Indirect = lists:umerge(lists:sort(IA), lists:sort(IB)),
    {Name, Direct -- Indirect, Indirect -- Direct}.

lcg_simplify_graph(LL) ->
    [lcg_demote_indirects(L, LL) || L <- LL].

lcg_demote_indirects({Name, Directs0, Indirects0}, LL) ->
    BeforeDirects = lcg_locked_before(Name, Directs0, LL, []),
    {Demoted, KeptDirects} =
        lists:partition(fun(Direct) ->
                                lists:member(Direct, BeforeDirects)
                        end,
                        Directs0),
    %% case Demoted of
    %%     [] -> ok;
    %%     _ -> io:format("Lock ~p demoted ~p\n", [Name, Demoted])
    %% end,
    {Name, KeptDirects, lists:usort(Indirects0 ++ Demoted)}.

lcg_locked_before(_This, [], _LL, Acc) ->
    lists:usort(Acc);
lcg_locked_before(This, [This|Tail], LL, Acc) ->
    lcg_locked_before(This, Tail, LL, Acc);
lcg_locked_before(This, [Name|Tail], LL, Acc) ->
    {Name, Directs0, _Indirects} = lists:keyfind(Name, 1, LL),
    Directs1 = lists:delete(Name, Directs0),
    DepthAcc = lcg_locked_before(Name, Directs1, LL, Acc),
    lcg_locked_before(This, Tail, LL, Directs1 ++ DepthAcc).

lcg_print(Out, LL) ->
    io:format(Out, "[", []),
    lcg_print_locks(Out, LL),
    io:format(Out, "].\n", []),
    ok.

lcg_print_locks(Out, [{_,_}=NoLock | Rest]) ->
    io:format(Out, "~p,\n", [NoLock]),
    lcg_print_locks(Out, Rest);
lcg_print_locks(Out, [LastLock]) ->
    io:format(Out, "~w", [LastLock]);
lcg_print_locks(Out, [Lock | Rest]) ->
    io:format(Out, "~w,\n", [Lock]),
    lcg_print_locks(Out, Rest).


%% Returns the amount of memory allocated by the given allocator type.
-spec alloc_blocks_size(Type) -> non_neg_integer() | undefined when
      Type :: atom().

alloc_blocks_size(Type) ->
    Allocs = erlang:system_info(alloc_util_allocators),
    Sizes = erlang:system_info({allocator_sizes, Allocs}),
    alloc_blocks_size_1(Sizes, Type, 0).

alloc_blocks_size_1([], _Type, 0) ->
    undefined;
alloc_blocks_size_1([{_Type, false} | Rest], Type, Acc) ->
    alloc_blocks_size_1(Rest, Type, Acc);
alloc_blocks_size_1([{_Type, Instances} | Rest], Type, Acc) ->
    F = fun ({instance, _, L}, Acc0) ->
                MBCSPool = case lists:keyfind(mbcs_pool, 1, L) of
                               {_, Pool} -> Pool;
                               false -> []
                           end,
                {_,MBCS} = lists:keyfind(mbcs, 1, L),
                {_,SBCS} = lists:keyfind(sbcs, 1, L),
                Acc1 = sum_block_sizes(MBCSPool, Type, Acc0),
                Acc2 = sum_block_sizes(MBCS, Type, Acc1),
                sum_block_sizes(SBCS, Type, Acc2)
        end,
    alloc_blocks_size_1(Rest, Type, lists:foldl(F, Acc, Instances));
alloc_blocks_size_1([], _Type, Acc) ->
    Acc.

sum_block_sizes([{blocks, List} | Rest], Type, Acc) ->
    sum_block_sizes(Rest, Type, sum_block_sizes_1(List, Type, Acc));
sum_block_sizes([_ | Rest], Type, Acc) ->
    sum_block_sizes(Rest, Type, Acc);
sum_block_sizes([], _Type, Acc) ->
    Acc.

sum_block_sizes_1([{Type, L} | Rest], Type, Acc0) ->
    Acc = lists:foldl(fun({size, Sz,_,_}, Sz0) -> Sz0+Sz;
                         ({size, Sz}, Sz0) -> Sz0+Sz;
                         (_, Sz) -> Sz
                      end, Acc0, L),
    sum_block_sizes_1(Rest, Type, Acc);
sum_block_sizes_1([_ | Rest], Type, Acc) ->
    sum_block_sizes_1(Rest, Type, Acc);
sum_block_sizes_1([], _Type, Acc) ->
    Acc.


