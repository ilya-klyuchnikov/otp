%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(observer_backend).
-moduledoc false.

%% General
-export([vsn/0]).

%% observer stuff
-export([socket_info/0,
	 sys_info/0, get_port_list/0, get_socket_list/0, procs_info/1,
	 get_table/3, get_table_list/2, fetch_stats/2]).

%% etop stuff
-export([etop_collect/1]).
-include("observer_backend.hrl").

%% ttb stuff
-export([ttb_init_node/3,
	 ttb_write_trace_info/3,
	 ttb_write_binary/2,
	 ttb_stop/1,
	 ttb_fetch/2,
	 ttb_fetch/3,
         ttb_resume_trace/0,
	 ttb_get_filenames/1]).

-define(CHUNKSIZE,8191). % 8 kbytes - 1 byte

-ifndef(ESOCK_DONT_SHOW_UNSUPPORTED_OPTS).
-define(ESOCK_KEEP_UNSUPPORTED_OPT(_),             true).
-else.
-define(ESOCK_KEEP_UNSUPPORTED_OPT(__SUPPORTED__), __SUPPORTED__).
-endif.

vsn() ->
    case application:load(runtime_tools) of
	R when R=:=ok; R=:={error,{already_loaded,runtime_tools}} -> 
	    application:get_key(runtime_tools,vsn);
	Error -> Error
    end.

%%
%% observer backend
%%

socket_info() ->
    Info0             = socket:info(),
    {Counters, Info1} = maps:take(counters, Info0),
    IovMax            = maps:get(iov_max, Info1),
    NumMons           = socket:number_of_monitors(),
    [{iov_max, IovMax}, {num_monitors, NumMons} | maps:to_list(Counters)].
    


sys_info() ->
    MemInfo = try erlang:memory() of
		  Mem -> Mem
	      catch _:_ -> []
	      end,

    SchedulersOnline = erlang:system_info(schedulers_online),
    SchedulersAvailable = case erlang:system_info(multi_scheduling) of
			      enabled -> SchedulersOnline;
			      _ -> 1
			  end,

    {{_,Input},{_,Output}} = erlang:statistics(io),
    [{uptime, element(1, erlang:statistics(wall_clock))},
     {run_queue, erlang:statistics(run_queue)},
     {io_input, Input},
     {io_output,  Output},

     {logical_processors, erlang:system_info(logical_processors)},
     {logical_processors_online, erlang:system_info(logical_processors_online)},
     {logical_processors_available, erlang:system_info(logical_processors_available)},
     {schedulers, erlang:system_info(schedulers)},
     {schedulers_online, SchedulersOnline},
     {schedulers_available, SchedulersAvailable},

     {otp_release, erlang:system_info(otp_release)},
     {version, erlang:system_info(version)},
     {system_architecture, erlang:system_info(system_architecture)},
     {kernel_poll, erlang:system_info(kernel_poll)},
     {smp_support, erlang:system_info(smp_support)},
     {threads, erlang:system_info(threads)},
     {thread_pool_size, erlang:system_info(thread_pool_size)},
     {wordsize_internal, erlang:system_info({wordsize, internal})},
     {wordsize_external, erlang:system_info({wordsize, external})},
     {alloc_info, alloc_info()},
     {process_count, erlang:system_info(process_count)},
     {atom_limit,  erlang:system_info(atom_limit)},
     {atom_count, erlang:system_info(atom_count)},
     {process_limit, erlang:system_info(process_limit)},
     {process_count, erlang:system_info(process_count)},
     {port_limit, erlang:system_info(port_limit)},
     {port_count, erlang:system_info(port_count)},
     {ets_limit,  erlang:system_info(ets_limit)},
     {ets_count, erlang:system_info(ets_count)},
     {dist_buf_busy_limit, erlang:system_info(dist_buf_busy_limit)}
     | MemInfo].

alloc_info() ->
    AlcuAllocs = erlang:system_info(alloc_util_allocators),
    try erlang:system_info({allocator_sizes, AlcuAllocs}) of
	Allocators -> Allocators
    catch _:_ -> []
    end.

get_table(Parent, Table, Module) ->
    spawn(fun() ->
		  link(Parent),
		  get_table2(Parent, Table, Module)
	  end).

get_table2(Parent, Table, Type) ->
    Size = case Type of
	       ets -> ets:info(Table, size);
	       mnesia -> mnesia:table_info(Table, size)
	   end,
    case Size =/= undefined andalso Size > 0 of
	false ->
	    Parent ! {self(), '$end_of_table'},
	    normal;
	true when Type =:= ets ->
	    Mem = ets:info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    get_ets_loop(Parent, ets:match(Table, '$1', NoElements));
	true ->
	    Mem = mnesia:table_info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    Ms = [{'$1', [], ['$1']}],
	    Get = fun() ->
			  get_mnesia_loop(Parent, mnesia:select(Table, Ms, NoElements, read))
		  end,
	    %% Not a transaction, we don't want to grab locks when inspecting the table
	    mnesia:async_dirty(Get)
    end.

get_ets_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_ets_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_ets_loop(Parent, ets:match(Cont)).

get_mnesia_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_mnesia_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_mnesia_loop(Parent, mnesia:select(Cont)).

get_port_list() ->
    ExtraItems = [monitors,monitored_by,parallelism,locking,queue_size,memory],
    PortInfo =
        fun(P, Acc) ->
                case erlang:port_info(P) of
                    undefined ->
                        Acc;
                    Info ->
                        [
                         [{port_id,P}|Info] ++
                             port_info(P,ExtraItems) ++
                             inet_port_extra(erlang:port_info(P, name), P)
                        | Acc ]
                end
        end,
    PIs = lists:foldl(PortInfo, [], erlang:ports()),
    lists:reverse(PIs).

port_info(P,[Item|Items]) ->
    case erlang:port_info(P,Item) of
        undefined -> port_info(P,Items);
        Value -> [Value|port_info(P,Items)]
    end;
port_info(_,[]) ->
    [].

inet_port_extra({_,Type},Port) when Type =:= "udp_inet";
                                    Type =:= "tcp_inet";
                                    Type =:= "sctp_inet" ->
    Data =
        case inet:getstat(Port) of
            {ok, Stats} -> [{statistics, Stats}];
            _ -> []
        end ++
        case inet:peername(Port) of
            {ok, {RAddr,RPort}} when is_tuple(RAddr), is_integer(RPort) ->
                [{remote_address,RAddr},{remote_port,RPort}];
            {ok, RAddr} ->
                [{remote_address,RAddr}];
            {error, _} ->  []
        end ++
        case inet:sockname(Port) of
            {ok, {LAddr,LPort}} when is_tuple(LAddr), is_integer(LPort) ->
                [{local_address,LAddr},{local_port,LPort}];
            {ok, LAddr} ->
                [{local_address,LAddr}];
            {error, _} -> []
        end ++
        [{options, get_sock_opts(Port)}],
    [{inet,Data}];
inet_port_extra(_,_) ->
    [].

sock_opts() ->
    [active, broadcast, buffer, bind_to_device,
     delay_send, deliver, dontroute, exit_on_close,
     header, high_msgq_watermark, high_watermark,
     ipv6_v6only, keepalive, linger, low_msgq_watermark,
     low_watermark, mode, netns, nodelay, packet,
     packet_size, priority, read_packets, recbuf,
     reuseaddr, send_timeout, send_timeout_close,
     show_econnreset, sndbuf, tos, tclass].

get_sock_opts(Port) ->
    get_sock_opts(Port, sock_opts()).

get_sock_opts(Port, Opts) ->
    get_sock_opts(Port, Opts, []).

%% The reason we are doing it this way, is because if there
%% is an issue with one of the options, we should just skip
%% that option and continue with the next.
%% Better to have some options then none.
get_sock_opts(_Port, [], Acc) ->
    lists:reverse(Acc);
get_sock_opts(Port, [Opt|Opts], Acc) ->
    case inet:getopts(Port, [Opt]) of
        {ok, [Res]} ->
            get_sock_opts(Port, Opts, [Res|Acc]);
        {ok, []} -> % No value?
            Res = {Opt, "-"},
            get_sock_opts(Port, Opts, [Res|Acc]);
        {error, einval} ->
            Res = {Opt, "Not Supported"},
            get_sock_opts(Port, Opts, [Res|Acc]);

        %% If the option is "invalid", the reason would be 'einval',
        %% so this error must be something else.
        %% But if the option just vanish, we don't know what is
        %% going on. So, do something similar to socket (see below).
        {error, Reason} ->
            Res = {Opt, f("error:~p", [Reason])},
            get_sock_opts(Port, Opts, [Res|Acc])
    end.


get_socket_list() ->
    GetOpt = fun(_Sock, {Opt, false}) ->
		     {Opt, "Not Supported"};
		(Sock, {Opt, true}) ->
		     case socket:getopt(Sock, Opt) of
			 %% Convert to string?
			 {ok, Value0} ->
			     %% d("get_socket_list -> ok: "
			     %% 	"~n   Option: ~p"
			     %% 	"~n   Value:  ~p", [Opt, Value]),
			     Value =
				 if
				     (Value0 =:= []) -> "-";
				     true -> Value0
				 end,
			     {Opt, Value};
			 %% We need to handle error cases and convert them
			 %% to something useful ("Not Supported")
			 {error, enotsup} = _ERROR ->
			     %% d("get_socket_list -> error: enotsup"),
			     {Opt, "Not Supported"};
			 {error, enoprotoopt} = _ERROR ->
			     %% d("get_socket_list -> error: enoprotoopt"),
			     {Opt, "Not Supported"};
			 {error, enotconn} = _ERROR ->
			     %% d("get_socket_list -> error: enotconn"),
			     {Opt, "Not Connected"};
			 {error, {invalid, _}} = _ERROR ->
			     %% d("get_socket_list -> error: invalid"),
			     {Opt, "Not Implemented"};
			 {error, Reason} ->
			     %% d("get_socket_list -> error: "
			     %% 	"~n   Option: ~p"
			     %% 	"~n   Reason: ~p", [Opt, _Reason]),
			     {Opt, f("error:~p", [Reason])}
		     end
	     end,
    [begin
	 Kind  = socket:which_socket_kind(S),
	 FD    = case socket:getopt(S, otp, fd) of
		     {ok, FD0} ->
			 FD0;
		     _ ->
			 -1
		 end,
	 Info0  = socket:info(S),
	 IdStr0 = socket:to_list(S),
	 IdStr  = case Info0 of
		      #{type     := stream,
			protocol := tcp} when (Kind =:= compat) ->
			  %% Faketi fake
			  "#Socket" ++ Id = IdStr0,
			  "#InetSocket" ++ Id;
		      _ ->
			  IdStr0
		  end,
	 {Counters0, Info1} = maps:take(counters, Info0),
	 Counters = maps:to_list(Counters0),
	 Info2 = maps:remove(ctype,         Info1),
	 Info3 = maps:remove(num_acceptors, Info2),
	 Info4 = maps:remove(num_readers,   Info3),
	 Info5 = maps:remove(num_writers,   Info4),
	 Info6 =
	     case socket:peername(S) of
		 {ok, RAddr} ->
		     RAddrStr = sockaddr_to_list(RAddr),
		     maps:put(raddress, RAddrStr, Info5);
		 _ ->
		     Info5
	     end,
	 Info7 =
	     case socket:sockname(S) of
		 {ok, LAddr} ->
		     LAddrStr = sockaddr_to_list(LAddr),
		     maps:put(laddress, LAddrStr, Info6);
		 _ ->
		     Info6
	     end,
	 SockOpts =
	     [{{socket, Opt}, Supported} ||
		 {Opt, Supported} <-
		     socket:supports(options, socket),
		 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)],
	 DomainOpts =
	     case Info7 of
		 #{domain := inet6} ->
		     [{{ipv6, Opt}, Supported} ||
			 {Opt, Supported} <-
			     socket:supports(options, ipv6),
			 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)];
		 _ ->
		     [{{ip, Opt}, Supported} ||
			 {Opt, Supported} <-
			     socket:supports(options, ip),
			 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)]
	     end,
	 ProtoOpts =
	     case Info7 of
		 #{domain   := Domain,
		   type     := stream,
		   protocol := tcp} when (Domain =:= inet) orelse
					 (Domain =:= inet6) ->
		     [{{tcp, Opt}, Supported} ||
			 {Opt, Supported} <-
			     socket:supports(options, tcp),
			 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)];
		 #{domain   := Domain,
		   type     := dgram,
		   protocol := udp} when (Domain =:= inet) orelse
					 (Domain =:= inet6) ->
		     [{{udp, Opt}, Supported} ||
			 {Opt, Supported} <-
			     socket:supports(options, udp),
			 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)];
		 #{domain   := Domain,
		   type     := seqpacket,
		   protocol := sctp} when (Domain =:= inet) orelse
					  (Domain =:= inet6) ->
		     [{{sctp, Opt}, Supported} ||
			 {Opt, Supported} <-
			     socket:supports(options, sctp),
			 ?ESOCK_KEEP_UNSUPPORTED_OPT(Supported)];
		 _ ->
		     []
	     end,
	 Opts    = SockOpts ++ DomainOpts ++ ProtoOpts,
	 Options = [GetOpt(S, Opt) || Opt <- Opts],
	 %% d("get_socket_list -> "
	 %%   "~n   Options:  ~p", [Options]),
	 Info7#{id           => S,
		id_str       => IdStr,
		fd           => FD,
		kind         => Kind,
		monitored_by => socket:monitored_by(S),
		statistics   => Counters,
		options      => lists:sort(Options)}
     end || S <- socket:which_sockets()].

sockaddr_to_list(#{family := local, path := PathBin}) when is_binary(PathBin) ->
    erlang:binary_to_list(PathBin);
sockaddr_to_list(#{family := local, path := Path}) when is_list(Path) ->
    Path;
sockaddr_to_list(#{family := inet, addr := Addr, port := Port}) ->
    inet_parse:ntoa(Addr) ++ " : " ++ erlang:integer_to_list(Port);
sockaddr_to_list(#{family := inet6, addr := Addr, port := Port,
		   flowinfo := FI, scope_id := SID}) ->
    inet_parse:ntoa(Addr) ++ " : " ++
	erlang:integer_to_list(Port) ++ 
	" , " ++ erlang:integer_to_list(FI) ++
	" , " ++ erlang:integer_to_list(SID);
sockaddr_to_list(Addr) ->
    f("~p", [Addr]).

-dialyzer({no_opaque_union, [get_ets_tab_id/1]}).
get_ets_tab_id(Id) ->
    case ets:info(Id, named_table) of
        true -> ignore;
        false -> Id
    end.

get_table_list(ets, Opts) ->
    HideUnread = proplists:get_value(unread_hidden, Opts, true),
    HideSys = proplists:get_value(sys_hidden, Opts, true),
    Info = fun(Id, Acc) ->
		   try
		       Name = ets:info(Id, name),
		       Protection = ets:info(Id, protection),
		       ignore(HideUnread andalso Protection == private, unreadable),
		       Owner = ets:info(Id, owner),
		       RegName = case catch process_info(Owner, registered_name) of
				     [] -> ignore;
				     {registered_name, ProcName} -> ProcName
				 end,
		       ignore(HideSys andalso ordsets:is_element(RegName, sys_processes()), system_tab),
		       ignore(HideSys andalso ordsets:is_element(Name, sys_tables()), system_tab),
		       ignore((RegName == mnesia_monitor)
			      andalso Name /= schema
			      andalso is_atom((catch mnesia:table_info(Name, where_to_read))), mnesia_tab),
		       Memory = ets:info(Id, memory) * erlang:system_info(wordsize),
		       Tab = [{name,Name},
			      {id,get_ets_tab_id(Id)},
			      {protection,Protection},
			      {owner,Owner},
			      {size,ets:info(Id, size)},
			      {reg_name,RegName},
			      {type,ets:info(Id, type)},
			      {keypos,ets:info(Id, keypos)},
			      {heir,ets:info(Id, heir)},
			      {memory,Memory},
			      {compressed,ets:info(Id, compressed)},
			      {fixed,ets:info(Id, fixed)}
			     ],
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~n",[Id, _What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], ets:all());

get_table_list(mnesia, Opts) ->
    HideSys = proplists:get_value(sys_hidden, Opts, true),
    Owner = ets:info(schema, owner),
    Owner /= undefined orelse
	throw({error, "Mnesia is not running on: " ++ atom_to_list(node())}),
    {registered_name, RegName} = process_info(Owner, registered_name),
    Info = fun(Id, Acc) ->
		   try
		       Name = Id,
		       ignore(HideSys andalso ordsets:is_element(Name, mnesia_tables()), system_tab),
		       ignore(Name =:= schema, mnesia_tab),
		       Storage = mnesia:table_info(Id, storage_type),
		       Tab0 = [{name,Name},
			       {owner,Owner},
			       {size,mnesia:table_info(Id, size)},
			       {reg_name,RegName},
			       {type,mnesia:table_info(Id, type)},
			       {keypos,2},
			       {memory,mnesia:table_info(Id, memory) * erlang:system_info(wordsize)},
			       {storage,Storage},
			       {index,mnesia:table_info(Id, index)}
			      ],
		       Tab = if Storage == disc_only_copies ->
				     [{fixed, dets:info(Id, safe_fixed)}|Tab0];
				(Storage == ram_copies) orelse
				(Storage == disc_copies) ->
				     [{fixed, ets:info(Id, fixed)},
				      {compressed, ets:info(Id, compressed)}|Tab0];
				true -> Tab0
			     end,
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~p ~n",[Id, _What, Stacktrace]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], mnesia:system_info(tables)).

fetch_stats(Parent, Time) ->
    process_flag(trap_exit, true),
    fetch_stats_loop(Parent, Time).

fetch_stats_loop(Parent, Time) ->
    erlang:system_flag(scheduler_wall_time, true),
    receive
	_Msg ->
	    erlang:system_flag(scheduler_wall_time, false),
	    ok
    after Time ->
	    _M = Parent ! {stats, 1,
			   erlang:statistics(scheduler_wall_time),
			   erlang:statistics(io),
			   try erlang:memory() catch _:_ -> [] end},
	    fetch_stats_loop(Parent, Time)
    end.

%%
%% Chunk sending process info to etop/observer
%%
procs_info(Collector) ->
    Iter0 = erlang:processes_iterator(),
    Limit = 10000,
    Send = fun Send(Iter1, Count) ->
                   case etop_collect(Iter1, Count, Limit, []) of
                       {none, ProcInfo} ->
                           Collector ! {procs_info, self(), ProcInfo};
                       {Iter, ProcInfo} ->
                           Collector ! {procs_info, self(), ProcInfo},
                           Send(Iter, 0)
                   end
           end,
    Send(Iter0, 0).

%%
%% etop backend
%%
etop_collect(Collector) ->
    %% If this is the first time and the scheduler_wall_time flag is
    %% false, SchedulerWallTime will be 'undefined' (and show 0 cpu
    %% utilization in etop). Next time the flag will be true and then
    %% there will be a measurement.
    SchedulerWallTime = erlang:statistics(scheduler_wall_time),
    Iter = erlang:processes_iterator(),
    {none, ProcInfo} = etop_collect(Iter, 0, infinity, []),

    Collector ! {self(),#etop_info{now = erlang:timestamp(),
				   n_procs = length(ProcInfo),
				   run_queue = erlang:statistics(run_queue),
				   runtime = SchedulerWallTime,
				   memi = etop_memi(),
				   procinfo = ProcInfo
				  }},

    case SchedulerWallTime of
	undefined ->
            spawn(fun() -> flag_holder_proc(Collector) end),
            ok;
	_ ->
	    ok
    end.

flag_holder_proc(Collector) ->
    erlang:system_flag(scheduler_wall_time,true),
    Ref = erlang:monitor(process,Collector),
    receive
	{'DOWN',Ref,_,_,_} ->
	    erlang:system_flag(scheduler_wall_time,false),
	    ok
    end.

etop_memi() ->
    try
	[{total, c:memory(total)},
	 {processes, c:memory(processes)}, 
	 {ets, c:memory(ets)},
	 {atom, c:memory(atom)},
	 {code, c:memory(code)},
	 {binary, c:memory(binary)}]
    catch
	error:notsup ->
	    undefined
    end.

etop_collect({P, Iter}, Count, Limit, Acc) when P =:= self() ->
    etop_collect(erlang:processes_next(Iter), Count, Limit, Acc);
etop_collect(Iter, Limit, Limit, Acc) ->
    {Iter, Acc};
etop_collect(none, _Count, _Limit, Acc) ->
    {none, Acc};
etop_collect({P, Iter}, Count, Limit, Acc) when is_pid(P) ->
    Fs = [registered_name,initial_call,
          {dictionary, '$initial_call'}, {dictionary, '$process_label'},
          memory,reductions,current_function,message_queue_len],
    case process_info(P, Fs) of
	undefined ->
	    etop_collect(erlang:processes_next(Iter), Count + 1, Limit, Acc);
	[{registered_name,Reg},{initial_call,Initial},
         {{dictionary, '$initial_call'}, DictInitial},
         {{dictionary, '$process_label'}, ProcId},
	 {memory,Mem},{reductions,Reds},
         {current_function,Current},{message_queue_len,Qlen}
        ] ->
	    Name = if Reg /= "" -> Reg;
                      ProcId /= undefined -> id_to_binary(ProcId);
                      true -> initial_call(Initial, DictInitial)
		   end,
	    Info = #etop_proc_info{pid=P,mem=Mem,reds=Reds,name=Name,
				   cf=Current,mq=Qlen},
	    etop_collect(erlang:processes_next(Iter), Count + 1, Limit, [Info|Acc])
    end;
etop_collect(Iter, Count, Limit, Acc) ->
    etop_collect(erlang:processes_next(Iter), Count, Limit, Acc).

id_to_binary(Id) when is_list(Id); is_binary(Id) ->
    try unicode:characters_to_binary(Id) of
        {error, _, _} ->
            unicode:characters_to_binary(io_lib:format("~0.tp", [Id]));
        BinString ->
            BinString
    catch _:_ ->
            unicode:characters_to_binary(io_lib:format("~0.tp", [Id]))
    end;
id_to_binary(TermId) ->
    unicode:characters_to_binary(io_lib:format("~0.tp", [TermId])).

initial_call({proc_lib, init_p, _}, DictInitial) ->
    case DictInitial of
        {_,_,_} = MFA ->
            MFA;
        undefined -> %% Fetch the default initial call
            proc_lib:translate_initial_call([])
    end;
initial_call(Initial, _Pid) ->
    Initial.

%%
%% ttb backend
%%
ttb_init_node(MetaFile_0,PI,Traci) ->
    if
	is_list(MetaFile_0);
	is_atom(MetaFile_0) ->
	    {ok, Cwd} = file:get_cwd(),
	    MetaFile = filename:join(Cwd, MetaFile_0),
	    file:delete(MetaFile);
	true -> 				% {local,_,_}
	    MetaFile = MetaFile_0
    end,
    case proplists:get_value(resume, Traci) of
        {true, _} -> (autostart_module()):write_config(Traci);
        _    -> ok
    end,
    Self = self(),
    MetaPid = spawn(fun() -> ttb_meta_tracer(MetaFile,PI,Self,Traci) end),
    receive {MetaPid,started} -> ok end,
    MetaPid ! {metadata,Traci},
    case PI of
	true ->
	    MetaPid ! {metadata,pnames()},
            ok;
	false ->
	    ok
    end,
    {ok,MetaFile,MetaPid}.

ttb_write_trace_info(MetaPid,Key,What) ->
    MetaPid ! {metadata,Key,What},
    ok.

ttb_meta_tracer(MetaFile,PI,Parent,SessionData) ->
    erlang:monitor(process, proplists:get_value(ttb_control, SessionData)),
    case PI of
	true ->
	    ReturnMS = [{'_',[],[{return_trace}]}],
	    erlang:trace_pattern({erlang,spawn,3},ReturnMS,[meta]),
	    erlang:trace_pattern({erlang,spawn_link,3},ReturnMS,[meta]),
	    erlang:trace_pattern({erlang,spawn_opt,4},ReturnMS,[meta]),
	    erlang:trace_pattern({erts_internal,spawn_init,1},[],[meta]),
	    erlang:trace_pattern({erts_internal,dist_spawn_init,1},[],[meta]),
	    erlang:trace_pattern({erlang,register,2},[],[meta]),
	    erlang:trace_pattern({global,register_name,2},[],[meta]),
            ok;
	false ->
	    ok
    end,
    Parent ! {self(),started},
    case proplists:get_value(overload_check, SessionData) of
        {Ms, M, F} ->
            catch M:F(init),
            erlang:send_after(Ms, self(), overload_check),
            ok;
        _ ->
            ok
    end,
    ttb_meta_tracer_loop(MetaFile,PI,dict:new(),SessionData).

ttb_meta_tracer_loop(MetaFile,PI,Acc,State) ->
    receive
	{trace_ts,_,call,{erlang,register,[Name,Pid]},_} ->
	    ok = ttb_store_meta({pid,{Pid,Name}},MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);
	{trace_ts,_,call,{global,register_name,[Name,Pid]},_} ->
	    ok = ttb_store_meta({pid,{Pid,{global,Name}}},MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);
	{trace_ts,CallingPid,call,{erlang,spawn_opt,[M,F,Args,_]},_} ->
	    MFA = {M,F,length(Args)},
	    NewAcc = dict:update(CallingPid,
				 fun(Old) -> [MFA|Old] end, [MFA], 
				 Acc),
	    ttb_meta_tracer_loop(MetaFile,PI,NewAcc,State);
	{trace_ts,CallingPid,return_from,{erlang,spawn_opt,_Arity},Ret,_} ->
	    case Ret of
		{NewPid,_Mref} when is_pid(NewPid) -> ok;
		NewPid when is_pid(NewPid) -> ok
	    end,
	    NewAcc = 
		dict:update(CallingPid,
			    fun([H|T]) -> 
				    ok = ttb_store_meta({pid,{NewPid,H}},MetaFile),
				    T 
			    end,
			    Acc),
	    ttb_meta_tracer_loop(MetaFile,PI,NewAcc,State);
	{trace_ts,CallingPid,call,{erlang,Spawn,[M,F,Args]},_} 
	when Spawn==spawn;Spawn==spawn_link ->
	    MFA = {M,F,length(Args)},
	    NewAcc = dict:update(CallingPid,
				 fun(Old) -> [MFA|Old] end, [MFA], 
				 Acc),
	    ttb_meta_tracer_loop(MetaFile,PI,NewAcc,State);

	{trace_ts,CallingPid,return_from,{erlang,Spawn,_Arity},NewPid,_} 
	when Spawn==spawn;Spawn==spawn_link ->
	    NewAcc = 
		dict:update(CallingPid,
			    fun([H|T]) -> 
				    ok = ttb_store_meta({pid,{NewPid,H}},MetaFile),
				    T
			    end,
			    Acc),
	    ttb_meta_tracer_loop(MetaFile,PI,NewAcc,State);

	{trace_ts,CallingPid,call,{erts_internal,spawn_init,[{M,F,Args}]},_} ->
            %% Local spawn_request()...
            ok = ttb_store_meta({pid,{CallingPid,{M,F,length(Args)}}},MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);

	{trace_ts,CallingPid,call,{erts_internal, dist_spawn_init, [MFnoA]},_} ->
            %% Distributed spawn_request()...
            ok = ttb_store_meta({pid,{CallingPid,MFnoA}},MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);

	{metadata,Data} when is_list(Data) ->
	    ok = ttb_store_meta(Data,MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);

	{metadata,Key,Fun} when is_function(Fun) ->
	    ok = ttb_store_meta([{Key,Fun()}],MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);

	{metadata,Key,What} ->
	    ok = ttb_store_meta([{Key,What}],MetaFile),
	    ttb_meta_tracer_loop(MetaFile,PI,Acc,State);
        overload_check ->
            {Ms, M, F} = proplists:get_value(overload_check, State),
            case catch M:F(check) of
                true ->
                    erlang:trace(all, false, [all]),
                    ControlPid = proplists:get_value(ttb_control, State),
                    ControlPid ! {node_overloaded, node()},
                    catch M:F(stop),
                    ttb_meta_tracer_loop(MetaFile,PI,Acc,lists:keydelete(overload_check, 1, State));
                _ ->
                    erlang:send_after(Ms, self(), overload_check),
                    ttb_meta_tracer_loop(MetaFile,PI,Acc, State)
            end;
     {'DOWN', _, _, _, _} ->
            _ = stop_seq_trace(),
            self() ! stop,
            ttb_meta_tracer_loop(MetaFile,PI,Acc, State);
     stop when PI=:=true ->
            try_stop_resume(State),
            try_stop_overload_check(State),
            erlang:trace_pattern({erlang,spawn,3},false,[meta]),
	    erlang:trace_pattern({erlang,spawn_link,3},false,[meta]),
	    erlang:trace_pattern({erlang,spawn_opt,4},false,[meta]),
	    erlang:trace_pattern({erts_internal,spawn_init,1},false,[meta]),
	    erlang:trace_pattern({erts_internal,dist_spawn_init,1},false,[meta]),
	    erlang:trace_pattern({erlang,register,2},false,[meta]),
	    erlang:trace_pattern({global,register_name,2},false,[meta]);
	stop ->
            try_stop_resume(State),
            try_stop_overload_check(State)
    end.

try_stop_overload_check(State) ->
    case proplists:get_value(overload, State) of
        undefined -> ok;
        {_, M, F} -> catch M:F(stop)
    end.

pnames() ->
    Processes = processes(),
    Globals = lists:map(fun(G) -> {global:whereis_name(G),G} end, 
			global:registered_names()),
    lists:flatten(lists:foldl(fun(Pid,Acc) -> [pinfo(Pid,Globals)|Acc] end, 
			      [], Processes)).

pinfo(P,Globals) ->
    case process_info(P,registered_name) of
	[] ->
	    case lists:keysearch(P,1,Globals) of
		{value,{P,G}} -> {pid,{P,{global,G}}};
		false -> 
		    case process_info(P,initial_call) of
			{_,I} -> {pid,{P,I}};
			undefined -> [] % the process has terminated
		    end
	    end;
	{_,R} -> {pid,{P,R}};
	undefined -> [] % the process has terminated
    end.

autostart_module() ->
    element(2, application:get_env(runtime_tools, ttb_autostart_module)).

try_stop_resume(State) ->
    case proplists:get_value(resume, State) of
        true -> (autostart_module()):delete_config();
        _    -> ok
    end.

ttb_resume_trace() ->
    case (autostart_module()):read_config() of
        {error, _} ->
            ok;
        {ok, Data} ->
            Pid = proplists:get_value(ttb_control, Data),
            {_, Timeout} = proplists:get_value(resume, Data),
            case rpc:call(node(Pid), erlang, whereis, [ttb]) of
                Pid ->
                    Pid ! {noderesumed, node(), self()},
                    wait_for_fetch_ready(Timeout);
                _ ->
                    ok
            end,
            (autostart_module()):delete_config(),
            ok
    end.

wait_for_fetch_ready(Timeout) ->
    receive
        trace_resumed ->
            ok
    after Timeout ->
            ok
    end.

ttb_store_meta(Data,{local,MetaFile,Port}) when is_list(Data) ->
    ttb_send_to_port(Port,MetaFile,Data);
ttb_store_meta(Data,MetaFile) when is_list(Data) ->
    {ok,Fd} = file:open(MetaFile,[raw,append]),
    ttb_write_binary(Fd,Data),
    file:close(Fd);
ttb_store_meta(Data,MetaFile) ->
    ttb_store_meta([Data],MetaFile).

ttb_write_binary(Fd,[H|T]) ->
    ok = file:write(Fd,ttb_make_binary(H)),
    ttb_write_binary(Fd,T);
ttb_write_binary(_Fd,[]) ->
    ok.

ttb_send_to_port(Port,MetaFile,[H|T]) ->
    B1 = ttb_make_binary(H),
    B2 = term_to_binary({metadata,MetaFile,B1}),
    erlang:port_command(Port,B2),
    ttb_send_to_port(Port,MetaFile,T);
ttb_send_to_port(_Port,_MetaFile,[]) ->
    ok.

ttb_make_binary(Term) ->
    B = term_to_binary(Term),
    SizeB = byte_size(B),
    if SizeB > 255 ->
	    %% size is bigger than 8 bits, must therefore add an extra
	    %% size field
	    SB = term_to_binary({'$size',SizeB}),
	    <<(byte_size(SB)):8, SB/binary, B/binary>>;
        true ->
	    <<SizeB:8, B/binary>>
    end.

    
%% Stop ttb
ttb_stop(MetaPid) ->
    Delivered = erlang:trace_delivered(all),
    receive
	{trace_delivered,all,Delivered} -> ok
    end,
    Ref = erlang:monitor(process,MetaPid),
    MetaPid ! stop,

    %% Must wait for the process to terminate there
    %% because dbg will be stopped when this function
    %% returns, and then the Port (in {local,MetaFile,Port})
    %% cannot be accessed any more.
    receive {'DOWN', Ref, process, MetaPid, _Info} -> ok end,
    stop_seq_trace().

stop_seq_trace() ->
    seq_trace:reset_trace(),
    seq_trace:set_system_tracer(false).

%% Fetch ttb logs from remote node
ttb_fetch(MetaFile,{Port,Host}) ->
    ttb_fetch(MetaFile,{Port,Host},undefined).
ttb_fetch(MetaFile,{Port,Host},MasterEnc) ->
    erlang:process_flag(priority,low),
    Files = ttb_get_filenames(MetaFile),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),
    send_files({Sock,Host},Files,MasterEnc,file:native_name_encoding()),
    ok = gen_tcp:close(Sock).


send_files({Sock,Host},[File|Files],MasterEnc,MyEnc) ->
    {ok,Fd} = file:open(File,[raw,read,binary]),
    Basename = filename:basename(File),
    {Code,FilenameBin} = encode_filename(Basename,MasterEnc,MyEnc),
    ok = gen_tcp:send(Sock,<<Code,FilenameBin/binary>>),
    send_chunks(Sock,Fd),
    ok = file:delete(File),
    send_files({Sock,Host},Files,MasterEnc,MyEnc);
send_files({_Sock,_Host},[],_MasterEnc,_MyEnc) ->
    done.

encode_filename(Basename,undefined,MyEnc) ->
    %% Compatible with old version of ttb.erl, but no longer crashing
    %% for code points > 255.
    {1,unicode:characters_to_binary(Basename,MyEnc,MyEnc)};
encode_filename(Basename,MasterEnc,MyEnc) ->
    case unicode:characters_to_binary(Basename,MyEnc,MasterEnc) of
        Bin when is_binary(Bin) ->
            %% Encoding succeeded
            {2,Bin};
        _ ->
            %% Can't convert Basename from my encoding to the master
            %% node's encoding. Doing my best and hoping that master
            %% node can fix it...
            {3,unicode:characters_to_binary(Basename,MyEnc,MyEnc)}
    end.

send_chunks(Sock,Fd) ->
    case file:read(Fd,?CHUNKSIZE) of
	{ok,Bin} -> 
	    ok = gen_tcp:send(Sock, <<0,Bin/binary>>),
	    send_chunks(Sock,Fd);
	eof ->
	    ok;
	{error,Reason} ->
	    ok = gen_tcp:send(Sock, <<2,(term_to_binary(Reason))/binary>>)
    end.

ttb_get_filenames(MetaFile) ->
    Dir = filename:dirname(MetaFile),
    Root = filename:rootname(filename:basename(MetaFile)),
    {ok,List} = file:list_dir(Dir),
    match_filenames(Dir,Root,List,[]).

match_filenames(Dir,MetaFile,[H|T],Files) ->
    case lists:prefix(MetaFile,H) of
	true -> match_filenames(Dir,MetaFile,T,[filename:join(Dir,H)|Files]);
	false -> match_filenames(Dir,MetaFile,T,Files)
    end;
match_filenames(_Dir,_MetaFile,[],Files) ->
    Files.


%%%%%%%%%%%%%%%%%

sys_tables() ->
    [ac_tab,  asn1,
     cdv_dump_index_table,  cdv_menu_table,  cdv_decode_heap_table,
     cell_id,  cell_pos,  clist,
     cover_internal_data_table,   cover_collected_remote_data_table, cover_binary_code_table,
     code, code_names,  cookies,
     corba_policy,  corba_policy_associations,
     dets, dets_owners, dets_registry,
     disk_log_names, disk_log_pids,
     eprof,  erl_atom_cache, erl_epmd_nodes,
     etop_accum_tab,  etop_tr,
     ets_coverage_data,
     file_io_servers,
     gs_mapping, gs_names,  gstk_db,
     gstk_grid_cellid, gstk_grid_cellpos, gstk_grid_id,
     httpd,
     id,
     ign_req_index, ign_requests,
     index,
     inet_cache, inet_db, inet_hosts,
     'InitialReferences',
     int_db,
     interpreter_includedirs_macros,
     ir_WstringDef,
     lmcounter,  locks,
						%     mnesia_decision,
     mnesia_gvar, mnesia_stats,
						%     mnesia_transient_decision,
     pg2_table,
     pg,
     queue,
     schema,
     shell_records,
     snmp_agent_table, snmp_local_db2, snmp_mib_data, snmp_note_store, snmp_symbolic_ets,
     tkFun, tkLink, tkPriv,
     ttb, ttb_history_table,
     udp_fds, udp_pids
    ].

sys_processes() ->
    [auth, code_server, global_name_server, inet_db,
     mnesia_recover, net_kernel, pg, timer_server, wxe_master].

mnesia_tables() ->
    [ir_AliasDef, ir_ArrayDef, ir_AttributeDef, ir_ConstantDef,
     ir_Contained, ir_Container, ir_EnumDef, ir_ExceptionDef,
     ir_IDLType, ir_IRObject, ir_InterfaceDef, ir_ModuleDef,
     ir_ORB, ir_OperationDef, ir_PrimitiveDef, ir_Repository,
     ir_SequenceDef, ir_StringDef, ir_StructDef, ir_TypedefDef,
     ir_UnionDef, logTable, logTransferTable, mesh_meas,
     mesh_type, mnesia_clist, orber_CosNaming,
     orber_objkeys, user
    ].

ignore(true, Reason) -> throw(Reason);
ignore(_,_ ) -> ok.

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% d(F) ->
%%     d(F, []).

%% d(Debug, F) when is_boolean(Debug) andalso is_list(F) ->
%%     d(Debug, F, []);
%% d(F, A) when is_list(F) andalso is_list(A) ->
%%     d(get(debug), F, A).

%% d(true, F, A) ->
%%     io:format("[ob] " ++ F ++ "~n", A);
%% d(_, _, _) ->
%%     ok.


