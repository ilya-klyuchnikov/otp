%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 30 Mar 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_callgraph).
-moduledoc false.

-export([add_edges/2,
	 add_edges/3,
	 all_nodes/1,
	 delete/1,
	 finalize/1,
	 is_escaping/2,
	 is_self_rec/2,
	 non_local_calls/1,
	 lookup_letrec/2,
	 lookup_rec_var/2,
	 lookup_call_site/2,
	 lookup_label/2,
	 lookup_name/2,
	 modules/1,
	 module_call_deps/1,
	 merge_module_deps/2,
	 module_postorder_from_funs/2,
	 new/0,
	 get_depends_on/2,
	 in_neighbours/2,
	 reset_from_funs/2,
	 scan_core_tree/2,
	 strip_module_deps/2,
	 remove_external/1,
	 mod_deps_to_dot/2,
	 mod_deps_to_ps/3,
	 to_dot/2,
	 to_ps/3]).

-export_type([callgraph/0, mfa_or_funlbl/0, callgraph_edge/0, mod_deps/0]).

-include("dialyzer.hrl").

%%----------------------------------------------------------------------

-type scc()	      :: [mfa_or_funlbl()].
-type mfa_call()      :: {mfa_or_funlbl(), mfa_or_funlbl()}.
-type mfa_calls()     :: [mfa_call()].

%%-----------------------------------------------------------------------------
%% A callgraph is a directed graph where the nodes are functions and a
%% call between two functions is an edge from the caller to the callee.
%%
%% calls	-  A mapping from call site (and apply site) labels
%%		   to the possible functions that can be called.
%% digraph	-  A digraph representing the callgraph.
%%		   Nodes are represented as MFAs or labels.
%% esc		-  A set of all escaping functions as reported by dialyzer_dep.
%% letrec_map	-  A dict mapping from letrec bound labels to function labels.
%%		   Includes all functions.
%% name_map	-  A mapping from label to MFA.
%% rev_name_map	-  A reverse mapping of the name_map.
%% rec_var_map	-  A dict mapping from letrec bound labels to function names.
%%		   Only for top level functions (from module defs).
%% self_rec	-  A set containing all self recursive functions.
%%		   Note that this contains MFAs for named functions and labels
%%		   whenever applicable.
%%-----------------------------------------------------------------------------

-record(callgraph, {digraph        = digraph:new() :: digraph:graph(),
		    active_digraph                 :: active_digraph() | 'undefined',
                    esc	                           :: ets:tid(),
                    letrec_map                     :: ets:tid(),
                    name_map	                   :: ets:tid(),
                    rev_name_map                   :: ets:tid(),
                    rec_var_map                    :: ets:tid(),
                    self_rec	                   :: ets:tid(),
                    calls                          :: ets:tid()}).


%% Exported Types

-opaque callgraph() :: #callgraph{}.

-type active_digraph() :: {'d', digraph:graph()}
                        | {'e',
                           Out :: ets:tid()}.

-type mod_deps() :: dict:dict(module(), [module()]).

%%----------------------------------------------------------------------

-spec new() -> callgraph().

new() ->
  [ETSEsc, ETSNameMap, ETSRevNameMap, ETSRecVarMap, ETSLetrecMap, ETSSelfRec, ETSCalls] =
    [ets:new(N,[public, {read_concurrency, true}]) ||
      N <- [callgraph_esc, callgraph_name_map, callgraph_rev_name_map,
	    callgraph_rec_var_map, callgraph_letrec_map, callgraph_self_rec, callgraph_calls]],
  #callgraph{esc            = ETSEsc,
	     letrec_map     = ETSLetrecMap,
	     name_map       = ETSNameMap,
	     rev_name_map   = ETSRevNameMap,
	     rec_var_map    = ETSRecVarMap,
	     self_rec       = ETSSelfRec,
	     calls          = ETSCalls}.

-spec delete(callgraph()) -> 'true'.

delete(#callgraph{digraph = Digraph}) ->
  digraph_delete(Digraph).

-spec all_nodes(callgraph()) -> [mfa()].

all_nodes(#callgraph{digraph = DG}) ->
  digraph_vertices(DG).

-spec lookup_rec_var(label(), callgraph()) -> 'error' | {'ok', mfa()}.

lookup_rec_var(Label, #callgraph{rec_var_map = RecVarMap})
  when is_integer(Label) ->
  ets_lookup_dict(Label, RecVarMap).

-spec lookup_letrec(label(), callgraph()) -> 'error' | {'ok', label()}.

lookup_letrec(Label, #callgraph{letrec_map = LetrecMap})
  when is_integer(Label) ->
  ets_lookup_dict(Label, LetrecMap).

-spec lookup_call_site(label(), callgraph()) -> 'error' | {'ok', [_]}. % XXX: refine

lookup_call_site(Label, #callgraph{calls = Calls})
  when is_integer(Label) ->
  ets_lookup_dict(Label, Calls).

-spec lookup_name(label(), callgraph()) -> 'error' | {'ok', mfa()}.

lookup_name(Label, #callgraph{name_map = NameMap})
  when is_integer(Label) ->
  ets_lookup_dict(Label, NameMap).

-spec lookup_label(mfa_or_funlbl(), callgraph()) -> 'error' | {'ok', integer()}.

lookup_label({_,_,_} = MFA, #callgraph{rev_name_map = RevNameMap}) ->
  ets_lookup_dict(MFA, RevNameMap);
lookup_label(Label, #callgraph{}) when is_integer(Label) ->
  {ok, Label}.

-spec in_neighbours(mfa_or_funlbl(), callgraph()) -> 'none' | [mfa_or_funlbl(),...].

in_neighbours(Label, #callgraph{digraph = Digraph} = CG)
  when is_integer(Label) ->
  Name = case lookup_name(Label, CG) of
	   {ok, Val} -> Val;
	   error -> Label
	 end,
  digraph_in_neighbours(Name, Digraph);
in_neighbours({_, _, _} = MFA, #callgraph{digraph = Digraph}) ->
  digraph_in_neighbours(MFA, Digraph).

-spec is_self_rec(mfa_or_funlbl(), callgraph()) -> boolean().

is_self_rec(MfaOrLabel, #callgraph{self_rec = SelfRecs}) ->
  ets_lookup_set(MfaOrLabel, SelfRecs).

-spec is_escaping(label(), callgraph()) -> boolean().

is_escaping(Label, #callgraph{esc = Esc}) when is_integer(Label) ->
  ets_lookup_set(Label, Esc).

-type callgraph_edge() :: {mfa_or_funlbl(),mfa_or_funlbl()}.
-spec add_edges([callgraph_edge()], callgraph()) -> ok.

add_edges([], _CG) ->
  ok;
add_edges(Edges, #callgraph{digraph = Digraph}) ->
  digraph_add_edges(Edges, Digraph).

-spec add_edges([callgraph_edge()], [mfa_or_funlbl()], callgraph()) -> ok.

add_edges(Edges, MFAs, #callgraph{digraph = DG} = CG) ->
  digraph_confirm_vertices(MFAs, DG),
  add_edges(Edges, CG).

-spec remove_external(callgraph()) -> {callgraph(), [tuple()]}.

remove_external(#callgraph{digraph = DG} = CG) ->
  {DG, External} = digraph_remove_external(DG),
  {CG, External}.

-spec non_local_calls(callgraph()) -> mfa_calls().

non_local_calls(#callgraph{digraph = DG}) ->
  Edges = digraph_edges(DG),
  find_non_local_calls(Edges, sets:new()).

-type call_tab() :: sets:set(mfa_call()).

-spec find_non_local_calls([{mfa_or_funlbl(), mfa_or_funlbl()}], call_tab()) ->
        mfa_calls().

find_non_local_calls([{{M,_,_}, {M,_,_}}|Left], Set) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{{M1,_,_}, {M2,_,_}} = Edge|Left], Set) when M1 =/= M2 ->
  find_non_local_calls(Left, sets:add_element(Edge, Set));
find_non_local_calls([{{_,_,_}, Label}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{Label, {_,_,_}}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{Label1, Label2}|Left], Set) when is_integer(Label1),
							is_integer(Label2) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([], Set) ->
  sets:to_list(Set).

%% Only considers call dependencies, not type dependencies, which are dealt with elsewhere
-spec get_depends_on(scc() | module(), callgraph()) -> [scc()].

get_depends_on(SCC, #callgraph{active_digraph = {'e', Out}}) ->
  case ets_lookup_dict(SCC, Out) of
    error -> [];
    {ok, Val} -> Val
  end;
get_depends_on(SCC, #callgraph{active_digraph = {'d', DG}}) ->
  digraph:out_neighbours(DG, SCC).

%% -spec get_required_by(scc() | module(), callgraph()) -> [scc()].

%% get_required_by(SCC, #callgraph{active_digraph = {'e', _Out, In, Maps}}) ->
%%   lookup_scc(SCC, In, Maps);
%% get_required_by(SCC, #callgraph{active_digraph = {'d', DG}}) ->
%%   digraph:in_neighbours(DG, SCC).


%%----------------------------------------------------------------------
%% Handling of modules & SCCs
%%----------------------------------------------------------------------

-spec modules(callgraph()) -> [module()].

modules(#callgraph{digraph = DG}) ->
  ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]).

-spec module_postorder(callgraph()) -> {[module()], {'d', digraph:graph()}}.

module_postorder(#callgraph{digraph = DG}) ->
  Edges = lists:foldl(fun edge_fold/2, sets:new(), digraph_edges(DG)),
  Modules = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new([acyclic]),
  digraph_confirm_vertices(Modules, MDG),
  Foreach = fun({M1,M2}) -> _ = digraph:add_edge(MDG, M1, M2) end,
  lists:foreach(Foreach, sets:to_list(Edges)),
  %% The out-neighbors of a vertex are the vertices called directly.
  %% The used vertices are to occur *before* the calling vertex:
  {lists:reverse(digraph_utils:topsort(MDG)), {'d', MDG}}.

edge_fold({{M1,_,_},{M2,_,_}}, Set) ->
  case M1 =/= M2 of
    true  -> sets:add_element({M1,M2},Set);
    false -> Set
  end;
edge_fold(_, Set) -> Set.


%% The module call deps of a module are modules that depend on the module to
%% make function calls
-spec module_call_deps(callgraph()) -> mod_deps().

module_call_deps(#callgraph{digraph = DG}) ->
  Edges = lists:foldl(fun edge_fold/2, sets:new(), digraph_edges(DG)),
  Modules = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new(),
  digraph_confirm_vertices(Modules, MDG),
  Foreach = fun({M1,M2}) -> check_add_edge(MDG, M1, M2) end,
  lists:foreach(Foreach, sets:to_list(Edges)),
  Deps = [{N, ordsets:from_list(digraph:in_neighbours(MDG, N))}
	  || N <- Modules],
  digraph_delete(MDG),
  dict:from_list(Deps).

-spec merge_module_deps(mod_deps(), mod_deps()) -> mod_deps().
merge_module_deps(Left, Right) ->
    dict:merge(
      fun (_Mod, L, R) ->
        gb_sets:to_list(gb_sets:union(
          gb_sets:from_list(L),
          gb_sets:from_list(R)))
      end,
      Left,
      Right).

-spec strip_module_deps(mod_deps(), sets:set(module())) -> mod_deps().

strip_module_deps(ModDeps, StripSet) ->
  FilterFun1 = fun(Val) -> not sets:is_element(Val, StripSet) end,
  MapFun = fun(_Key, ValSet) -> ordsets:filter(FilterFun1, ValSet) end,
  ModDeps1 = dict:map(MapFun, ModDeps),
  FilterFun2 = fun(_Key, ValSet) -> ValSet =/= [] end,
  dict:filter(FilterFun2, ModDeps1).

-spec finalize(callgraph()) -> {[scc()], callgraph()}.

finalize(#callgraph{digraph = DG} = CG) ->
  {ActiveDG, LabelledPostorder} = condensation(DG),
  {LabelledPostorder, CG#callgraph{active_digraph = ActiveDG}}.

-spec reset_from_funs([mfa_or_funlbl()], callgraph()) -> {[scc()], callgraph()}.

reset_from_funs(Funs, #callgraph{digraph = DG, active_digraph = ADG} = CG) ->
  active_digraph_delete(ADG),
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  {NewActiveDG, LabelledPostorder} = condensation(SubGraph),
  digraph_delete(SubGraph),
  {LabelledPostorder, CG#callgraph{active_digraph = NewActiveDG}}.

-spec module_postorder_from_funs([mfa_or_funlbl()], callgraph()) ->
        {[module()], callgraph()}.

module_postorder_from_funs(Funs, #callgraph{digraph = DG,
					    active_digraph = ADG} = CG) ->
  active_digraph_delete(ADG),
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  {PO, Active} = module_postorder(CG#callgraph{digraph = SubGraph}),
  digraph_delete(SubGraph),
  {PO, CG#callgraph{active_digraph = Active}}.

%% We KNOW that `error` is not a valid value in the table.
ets_lookup_dict(Key, Table) ->
  case ets:lookup_element(Table, Key, 2, error) of
    error -> error;
    Val -> {ok, Val}
  end.

ets_lookup_set(Key, Table) ->
  ets:member(Table, Key).

%%----------------------------------------------------------------------
%% Core code
%%----------------------------------------------------------------------

%% The core tree must be labeled as by cerl_trees:label/1 (or /2).
%% The set of labels in the tree must be disjoint from the set of
%% labels already occurring in the callgraph.

-spec scan_core_tree(cerl:c_module(), callgraph()) ->
        {[mfa_or_funlbl()], [callgraph_edge()]}.

scan_core_tree(Tree, #callgraph{calls = ETSCalls,
				esc = ETSEsc,
				letrec_map = ETSLetrecMap,
				name_map = ETSNameMap,
				rec_var_map = ETSRecVarMap,
				rev_name_map = ETSRevNameMap,
				self_rec = ETSSelfRec}) ->
  %% Build name map and recursion variable maps.
  build_maps(Tree, ETSRecVarMap, ETSNameMap, ETSRevNameMap, ETSLetrecMap),

  %% First find the module-local dependencies.
  {Deps0, EscapingFuns, Calls, Letrecs} = dialyzer_dep:analyze(Tree),
  true = ets:insert(ETSCalls, dict:to_list(Calls)),
  true = ets:insert(ETSLetrecMap, dict:to_list(Letrecs)),
  true = ets:insert(ETSEsc, [{E} || E <- EscapingFuns]),

  LabelEdges = get_edges_from_deps(Deps0),

  %% Find the self recursive functions. Named functions get both the
  %% key and their name for convenience.
  SelfRecs0 = lists:foldl(fun({Key, Key}, Acc) ->
			      case ets_lookup_dict(Key, ETSNameMap) of
				error      -> [Key|Acc];
				{ok, Name} -> [Key, Name|Acc]
			      end;
			     (_, Acc) -> Acc
			  end, [], LabelEdges),
  true = ets:insert(ETSSelfRec, [{S} || S <- SelfRecs0]),

  NamedEdges1 = name_edges(LabelEdges, ETSNameMap),

  %% We need to scan for inter-module calls since these are not tracked
  %% by dialyzer_dep. Note that the caller is always recorded as the
  %% top level function. This is OK since the included functions are
  %% stored as scc with the parent.
  NamedEdges2 = scan_core_funs(Tree),

  %% Confirm all nodes in the tree.
  Names1 = lists:append([[X, Y] || {X, Y} <- NamedEdges1]),
  Names2 = ordsets:from_list(Names1),

  %% Get rid of the 'top' function from nodes and edges.
  Names3 = ordsets:del_element(top, Names2),
  NewNamedEdges2 =
    [E || {From, To} = E <- NamedEdges2, From =/= top, To =/= top],
  NewNamedEdges1 =
    [E || {From, To} = E <- NamedEdges1, From =/= top, To =/= top],
  NamedEdges3 = NewNamedEdges1 ++ NewNamedEdges2,
  {Names3, NamedEdges3}.

build_maps(Tree, ETSRecVarMap, ETSNameMap, ETSRevNameMap, ETSLetrecMap) ->
  %% We only care about the named (top level) functions. The anonymous
  %% functions will be analysed together with their parents.
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  Fun =
    fun({Var, Function}) ->
	FunName = cerl:fname_id(Var),
	Arity = cerl:fname_arity(Var),
	MFA = {Mod, FunName, Arity},
	FunLabel = get_label(Function),
	VarLabel = get_label(Var),
	true = ets:insert(ETSLetrecMap, {VarLabel, FunLabel}),
	true = ets:insert(ETSNameMap, {FunLabel, MFA}),
	true = ets:insert(ETSRevNameMap, {MFA, FunLabel}),
	true = ets:insert(ETSRecVarMap, {VarLabel, MFA})
    end,
  lists:foreach(Fun, Defs).

get_edges_from_deps(Deps) ->
  %% Convert the dependencies as produced by dialyzer_dep to a list of
  %% edges. Also, remove 'external' since we are not interested in
  %% this information.
  Edges = dict:fold(fun(external, _Set, Acc) -> Acc;
		       (Caller, Set, Acc)    ->
			[[{Caller, Callee} || Callee <- Set,
					      Callee =/= external]|Acc]
		    end, [], Deps),
  lists:flatten(Edges).

name_edges(Edges, ETSNameMap) ->
  %% If a label is present in the name map it is renamed. Otherwise
  %% keep the label as the identity.
  MapFun = fun(X) ->
	       case ets_lookup_dict(X, ETSNameMap) of
		 error -> X;
		 {ok, MFA} -> MFA
	       end
	   end,
  name_edges(Edges, MapFun, []).

name_edges([{From, To}|Left], MapFun, Acc) ->
  NewFrom = MapFun(From),
  NewTo = MapFun(To),
  name_edges(Left, MapFun, [{NewFrom, NewTo}|Acc]);
name_edges([], _MapFun, Acc) ->
  Acc.

scan_core_funs(Tree) ->
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  DeepEdges = lists:foldl(fun({Var, Function}, Edges) ->
			      FunName = cerl:fname_id(Var),
			      Arity = cerl:fname_arity(Var),
			      MFA = {Mod, FunName, Arity},
			      [scan_one_core_fun(Function, MFA)|Edges]
			  end, [], Defs),
  lists:flatten(DeepEdges).

scan_one_core_fun(TopTree, FunName) ->
  FoldFun = fun(Tree, Acc) ->
		case cerl:type(Tree) of
		  call ->
		    CalleeM = cerl:call_module(Tree),
		    CalleeF = cerl:call_name(Tree),
		    CalleeArgs = cerl:call_args(Tree),
		    A = length(CalleeArgs),
		    case (cerl:is_c_atom(CalleeM) andalso
			  cerl:is_c_atom(CalleeF)) of
		      true ->
			M = cerl:atom_val(CalleeM),
			F = cerl:atom_val(CalleeF),
			case erl_bif_types:is_known(M, F, A) of
			  true ->
			    case {M, F, A} of
			      {erlang, make_fun, 3} ->
				[CA1, CA2, CA3] = CalleeArgs,
				case
				  cerl:is_c_atom(CA1) andalso
				  cerl:is_c_atom(CA2) andalso
				  cerl:is_c_int(CA3)
				of
				  true ->
				    MM = cerl:atom_val(CA1),
				    FF = cerl:atom_val(CA2),
				    AA = cerl:int_val(CA3),
				    case erl_bif_types:is_known(MM, FF, AA) of
				      true -> Acc;
				      false -> [{FunName, {MM, FF, AA}}|Acc]
				    end;
				  false ->
				    Acc
				end;
			      _ ->
				Acc
			    end;
			  false -> [{FunName, {M, F, A}}|Acc]
			end;
		      false ->
			%% We cannot handle run-time bindings
			Acc
		    end;
		  _ ->
		    %% Nothing that can introduce new edges in the callgraph.
		    Acc
		end
	    end,
  cerl_trees:fold(FoldFun, [], TopTree).

get_label(T) ->
  case cerl:get_ann(T) of
    [{label, L} | _] when is_integer(L) -> L;
    _ -> erlang:error({missing_label, T})
  end.

%%----------------------------------------------------------------------
%% Digraph
%%----------------------------------------------------------------------

digraph_add_edges([{From, To}|Left], DG) ->
  digraph_add_edge(From, To, DG),
  digraph_add_edges(Left, DG);
digraph_add_edges([], _DG) ->
  ok.

digraph_add_edge(From, To, DG) ->
  case digraph:vertex(DG, From) of
    false -> digraph:add_vertex(DG, From);
    {From, _} -> ok
  end,
  case digraph:vertex(DG, To) of
    false -> digraph:add_vertex(DG, To);
    {To, _} -> ok
  end,
  check_add_edge(DG, {From, To}, From, To, []),
  ok.

check_add_edge(G, V1, V2) ->
  case digraph:add_edge(G, V1, V2) of
    {error, Error} -> exit({add_edge, V1, V2, Error});
    _Edge -> ok
  end.

check_add_edge(G, E, V1, V2, L) ->
  case digraph:add_edge(G, E, V1, V2, L) of
    {error, Error} -> exit({add_edge, E, V1, V2, L, Error});
    _Edge -> ok
  end.

digraph_confirm_vertices([MFA|Left], DG) ->
  digraph:add_vertex(DG, MFA, confirmed),
  digraph_confirm_vertices(Left, DG);
digraph_confirm_vertices([], _DG) ->
  ok.

digraph_remove_external(DG) ->
  Vertices = digraph:vertices(DG),
  Unconfirmed = remove_unconfirmed(Vertices, DG),
  {DG, Unconfirmed}.

remove_unconfirmed(Vertices, DG) ->
  remove_unconfirmed(Vertices, DG, []).

remove_unconfirmed([V|Left], DG, Unconfirmed) ->
  case digraph:vertex(DG, V) of
    {V, confirmed} -> remove_unconfirmed(Left, DG, Unconfirmed);
    {V, []} -> remove_unconfirmed(Left, DG, [V|Unconfirmed])
  end;
remove_unconfirmed([], DG, Unconfirmed) ->
  BadCalls = lists:append([digraph:in_edges(DG, V) || V <- Unconfirmed]),
  BadCallsSorted = lists:keysort(1, BadCalls),
  digraph:del_vertices(DG, Unconfirmed),
  BadCallsSorted.

digraph_delete(DG) ->
  digraph:delete(DG).

active_digraph_delete({'d', DG}) ->
  digraph:delete(DG);
active_digraph_delete({'e', Out}) ->
  ets:delete(Out).

digraph_edges(DG) ->
  digraph:edges(DG).

digraph_vertices(DG) ->
  digraph:vertices(DG).

digraph_in_neighbours(V, DG) ->
  case digraph:in_neighbours(DG, V) of
    [] -> none;
    List -> List
  end.

digraph_reaching_subgraph(Funs, DG) ->
  Vertices = digraph_utils:reaching(Funs, DG),
  digraph_utils:subgraph(DG, Vertices).

%%=============================================================================
%% Utilities for 'dot'
%%=============================================================================

-spec mod_deps_to_dot(mod_deps(), file:filename()) -> 'ok'.

mod_deps_to_dot(ModDeps, File) ->
  DepEdges =
    lists:flatten(
    [
      [{Mod, ModuleDependingOnIt} || ModuleDependingOnIt <- ModulesDependingOnIt]
    || {Mod,ModulesDependingOnIt} <- dict:to_list(ModDeps)
    ]),
  dialyzer_dot:translate_list(DepEdges, File, "mod_deps").

-spec mod_deps_to_ps(mod_deps(), file:filename(), string()) -> 'ok'.

mod_deps_to_ps(ModDeps, File, Args) ->
  %% TODO: As with `to_dot/2`, handle Unicode names.
  DotFile = filename:rootname(File) ++ ".dot",
  mod_deps_to_dot(ModDeps, DotFile),
  Command = io_lib:format("dot -Tps ~ts -o ~ts ~ts", [Args, File, DotFile]),
  _ = os:cmd(Command),
  ok.

-spec to_dot(callgraph(), file:filename()) -> 'ok'.

to_dot(#callgraph{digraph = DG, esc = Esc} = CG, File) ->
  %% TODO: handle Unicode names.
  Fun = fun(L) ->
	    case lookup_name(L, CG) of
	      error -> L;
	      {ok, Name} -> Name
	    end
	end,
  Escaping = [{Fun(L), {color, red}}
	      || L <- [E || {E} <- ets:tab2list(Esc)], L =/= external],
  Vertices = digraph_edges(DG),
  dialyzer_dot:translate_list(Vertices, File, "CG", Escaping).

-spec to_ps(callgraph(), file:filename(), string()) -> 'ok'.

to_ps(#callgraph{} = CG, File, Args) ->
  %% TODO: handle Unicode names.
  Dot_File = filename:rootname(File) ++ ".dot",
  to_dot(CG, Dot_File),
  Command = io_lib:format("dot -Tps ~ts -o ~ts ~ts", [Args, File, Dot_File]),
  _ = os:cmd(Command),
  ok.

condensation(G) ->
  OutETS = ets:new(callgraph_label_deps_out,[{read_concurrency, true}]),
  SCCs = digraph_utils:strong_components(G),
  %% Assign unique numbers to SCCs:
  Ints = lists:seq(1, length(SCCs)),
  IntToSCC = lists:zip(Ints, SCCs),
  IntScc = sofs:relation(IntToSCC, [{int, scc}]),

  %% Subsitute strong components for vertices in edges using the
  %% unique numbers:
  C2V = sofs:relation([{SC, V} || SC <- SCCs, V <- SC], [{scc, v}]),
  I2V = sofs:relative_product(IntScc, C2V), % [{v, int}]
  Es = sofs:relation(digraph:edges(G), [{v, v}]),
  R1 = sofs:relative_product(I2V, Es),
  R2 = sofs:relative_product(I2V, sofs:converse(R1)),
  R2Strict = sofs:strict_relation(R2),
  %% Create out-neighbours:
  Out = sofs:relation_to_family(sofs:converse(R2Strict)),
  ets:insert(OutETS, sofs:to_external(Out)),
  %% Sort the SCCs topologically:
  DG = sofs:family_to_digraph(Out),
  lists:foreach(fun(I) -> digraph:add_vertex(DG, I) end, Ints),
  SCCInts0 = digraph_utils:topsort(DG),
  digraph:delete(DG),
  %% The out-neighbors of a vertex are the vertices called directly.
  %% The used vertices are to occur *before* the calling vertex:
  SCCInts = lists:reverse(SCCInts0),
  IntToSCCMap = maps:from_list(IntToSCC),
  LabelledPostorder = [{I, maps:get(I, IntToSCCMap)} || I <- SCCInts],
  {{'e', OutETS}, LabelledPostorder}.
