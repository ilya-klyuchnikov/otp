%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
-module(tls_socket).
-moduledoc false.

-behaviour(gen_server).

-include("ssl_internal.hrl").
-include("ssl_api.hrl").
-include("ssl_record.hrl").

-export([send/3, send/4,
         listen/3, 
         accept/3, 
         socket/6,
         connect/4, 
         upgrade/4,
	 setopts/3, 
         getopts/3, 
         getstat/3, 
         peername/2, 
         sockname/2, 
         port/2,
         close/2,
         monitor_socket/1]).

-export([split_options/2,
         get_socket_opts/3]).

-export([emulated_options/1,
         emulated_options/2,
         internal_inet_values/1,
         default_inet_values/1,
	 init/1, 
         start_link/3, 
         terminate/2, 
         inherit_tracker/3, 
	 emulated_socket_options/2, 
         get_emulated_opts/1, 
	 set_emulated_opts/2, 
         get_all_opts/1, 
         handle_call/3, 
         handle_cast/2,
	 handle_info/2, 
         code_change/3]).

-export([update_active_n/2]).

-record(state, {
	  emulated_opts,
          listen_monitor,      
	  ssl_opts
	 }).

%%--------------------------------------------------------------------
%%% Internal API
%%--------------------------------------------------------------------
send(Transport, Socket, Data) ->
    Transport:send(Socket, Data).

send(tls_socket_tcp, _Socket, [], _Handle) ->
    ok;
send(tls_socket_tcp, Socket, Data, Handle) ->
    tls_socket_tcp:send_async(Socket, Data, Handle);
send(Transport, Socket, Data, _Handle) ->
    Transport:send(Socket, Data).

listen(Transport, Port, #config{transport_info = {Transport, _, _, _, _}, 
				inet_user = Options, 
				ssl = SslOpts, emulated = EmOpts} = Config) ->
    case Transport:listen(Port, Options ++ internal_inet_values(Transport)) of
	{ok, ListenSocket} ->
	    {ok, Tracker} = inherit_tracker(ListenSocket, EmOpts, SslOpts),
            LifeTime = ssl_config:get_ticket_lifetime(),
            TicketStoreSize = ssl_config:get_ticket_store_size(),
            MaxEarlyDataSize = ssl_config:get_max_early_data_size(),
            %% TLS-1.3 session handling
            {ok, SessionHandler} =
                session_tickets_tracker(ListenSocket, LifeTime, TicketStoreSize, MaxEarlyDataSize, SslOpts),
            %% PRE TLS-1.3 session handling
            {ok, SessionIdHandle} = session_id_tracker(ListenSocket, SslOpts),
            Trackers =  [{option_tracker, Tracker}, {session_tickets_tracker, SessionHandler},
                         {session_id_tracker, SessionIdHandle}],
            Socket = #sslsocket{socket_handle = ListenSocket,
                                listener_config = Config#config{trackers = Trackers}},
            check_active_n(EmOpts, Socket),
	    {ok, Socket};
	Err = {error, _} ->
	    Err
    end.

accept(ListenSocket, #config{transport_info = {Transport,_,_,_,_} = CbInfo,
			     ssl = SslOpts,
			     trackers = Trackers}, Timeout) -> 
    case Transport:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
            Tracker = proplists:get_value(option_tracker, Trackers),
            {ok, EmOpts} = get_emulated_opts(Tracker),
	    {ok, Port} = tls_socket:port(Transport, Socket),
            start_tls_server_connection(SslOpts, Port, Socket, EmOpts, Trackers, CbInfo);
	{error, Reason} ->
	    {error, Reason}
    end.

upgrade(client, Socket, #config{transport_info = CbInfo,
                                ssl = SslOptions,
                                emulated = EmOpts}, Timeout) ->
    Transport = element(1, CbInfo),
    ok = setopts(Transport, Socket, internal_inet_values(Transport)),
    case peername(Transport, Socket) of
	{ok, {Host, Port}} ->
            start_tls_client_connection(Host, Port, Socket, SslOptions, EmOpts, CbInfo, Timeout);
	{error, Error} ->
	    {error, Error}
    end;
upgrade(server, Socket, #config{transport_info = CbInfo,
                                ssl = SslOpts,
                                emulated = EmOpts}, Timeout) ->
    Transport = element(1, CbInfo),
    ok = setopts(Transport, Socket, internal_inet_values(Transport)),
    {ok, Port} = port(Transport, Socket),
    {ok, SessionIdHandle} = session_id_tracker(ssl_unknown_listener, SslOpts),
    Trackers = [{session_id_tracker, SessionIdHandle}],
    {ok, SSocket} = start_tls_server_connection(SslOpts, Port, Socket, EmOpts, Trackers, CbInfo),
    ssl_gen_statem:handshake(SSocket, Timeout).

connect(Host, Port,
	#config{transport_info = CbInfo, inet_user = UserOpts, ssl = SslOpts,
		emulated = EmOpts, inet_ssl = SocketOpts},
	Timeout) ->
    {Transport, _, _, _, _} = CbInfo,
    try Transport:connect(Host, Port, SocketOpts ++ internal_inet_values(Transport), Timeout) of
	{ok, Socket} ->
	    start_tls_client_connection(Host, Port, Socket, SslOpts, EmOpts, CbInfo, Timeout);
	{error, Reason} ->
	    {error, Reason}
    catch
	exit:{function_clause, _} ->
	    {error, {badarg, connect_error(Transport, Host, Port, UserOpts, Timeout)}};
        exit:badarg ->
	    {error, {badarg, connect_error(Transport, Host, Port, UserOpts, Timeout)}};
	exit:{badarg, Reason} ->
	    {error, {badarg, connect_error(Transport, Host, Port, UserOpts, Timeout), Reason}}
    end.

socket([Receiver, Sender], Transport, Socket, ConnectionCb, Tab, Trackers) ->
    #sslsocket{socket_handle = Socket,
               connection_handler = Receiver,
               payload_sender = Sender,
               transport_cb = Transport,
               connection_cb = ConnectionCb,
               tab = Tab,
               listener_config = Trackers}.

setopts(gen_tcp, Socket = #sslsocket{socket_handle = ListenSocket, 
                                     listener_config = #config{trackers = Trackers}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOpts, EmulatedOpts} = split_options(gen_tcp, Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    check_active_n(EmulatedOpts, Socket),
    inet:setopts(ListenSocket, SockOpts);
setopts(Transport, Socket = #sslsocket{socket_handle = ListenSocket, 
                               listener_config = #config{transport_info = Info,
                                                         trackers = Trackers}}, Options) ->
    Transport = element(1, Info),
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOpts, EmulatedOpts} = split_options(Transport, Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    check_active_n(EmulatedOpts, Socket),
    Transport:setopts(ListenSocket, SockOpts);
%%% Following clauses will not be called for emulated options, they are handled in the connection process
setopts(gen_tcp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

check_active_n(EmulatedOpts, Socket = #sslsocket{listener_config = #config{trackers = Trackers}}) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    %% We check the resulting options to send an ssl_passive message if necessary.
    case proplists:lookup(active, EmulatedOpts) of
        %% The provided value is out of bound.
        {_, N} when is_integer(N), N < -32768 ->
            throw(einval);
        {_, N} when is_integer(N), N > 32767 ->
            throw(einval);
        {_, N} when is_integer(N) ->
            case get_emulated_opts(Tracker, [active]) of
                [{_, false}] ->
                    self() ! {ssl_passive, Socket},
                    ok;
                %% The result of the addition is out of bound.
                [{_, A}] when is_integer(A), A < -32768 ->
                    throw(einval);
                [{_, A}] when is_integer(A), A > 32767 ->
                    throw(einval);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

getopts(gen_tcp,  #sslsocket{socket_handle = ListenSocket, 
                             listener_config = #config{trackers = Trackers}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOptNames, EmulatedOptNames} = split_options(gen_tcp, Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, inet),
    {ok, EmulatedOpts ++ SocketOpts}; 
getopts(Transport,  #sslsocket{socket_handle = ListenSocket, 
                               listener_config = #config{trackers = Trackers}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOptNames, EmulatedOptNames} = split_options(Transport, Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, Transport),
    {ok, EmulatedOpts ++ SocketOpts}; 
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
getopts(gen_tcp, Socket, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).

getstat(gen_tcp, Socket, Options) ->
	inet:getstat(Socket, Options);
getstat(Transport, Socket, Options) ->
	Transport:getstat(Socket, Options).

peername(gen_tcp, Socket) ->
    inet:peername(Socket);
peername(Transport, Socket) ->
    Transport:peername(Socket).

sockname(gen_tcp, Socket) ->
    inet:sockname(Socket);
sockname(Transport, Socket) ->
    Transport:sockname(Socket).

port(gen_tcp, Socket) ->
    inet:port(Socket);
port(Transport, Socket) ->
    Transport:port(Socket).

close(gen_tcp, Socket) ->
    inet:close(Socket);
close(Transport, Socket) ->
    Transport:close(Socket).

monitor_socket({'$socket', _}=Socket) ->
    socket:monitor(Socket);
monitor_socket(InetSocket) ->
    inet:monitor(InetSocket).

emulated_options(tls_socket_tcp) ->
    [mode, packet, active, header, packet_size, high_watermark, low_watermark];
emulated_options(_) ->
    [mode, packet, active, header, packet_size].

emulated_options(Transport, Opts) ->
    emulated_options(Opts, [], default_inet_values(Transport)).

internal_inet_values(tls_socket_tcp) ->
    [];
internal_inet_values(_) ->
    [{packet_size,0}, {packet, 0}, {header, 0}, {active, false}, {mode,binary}].

default_inet_values(gen_tcp) ->
    [{packet_size, 0}, {packet,0}, {header, 0}, {active, true}, {mode, list}];
default_inet_values(tls_socket_tcp) ->
    [{packet_size, 0}, {packet,0}, {header, 0}, {active, true}, {mode, list},
     {high_watermark, 8192}, {low_watermark, 4096}].

inherit_tracker(ListenSocket, EmOpts, #{erl_dist := true} = SslOpts) ->
    ssl_listen_tracker_sup:start_child_dist([ListenSocket, EmOpts, SslOpts]);
inherit_tracker(ListenSocket, EmOpts, SslOpts) ->
    ssl_listen_tracker_sup:start_child([ListenSocket, EmOpts, SslOpts]).

session_tickets_tracker(ListenSocket, Lifetime, TicketStoreSize, MaxEarlyDataSize,
                        #{erl_dist := true,
                          session_tickets := Mode,
                          anti_replay := AntiReplay,
                          stateless_tickets_seed := Seed}) ->
    SupName = tls_server_session_ticket_sup:sup_name(dist),
    Children = supervisor:count_children(SupName),
    Workers = proplists:get_value(workers, Children),
    case Workers of
        0 ->
            tls_server_session_ticket_sup:start_child([ListenSocket, Mode, Lifetime,
                                                       TicketStoreSize, MaxEarlyDataSize,
                                                       AntiReplay, Seed]);
        1 ->
            [{_,Child,_, _}] = supervisor:which_children(SupName),
            {ok, Child}
    end;
session_tickets_tracker(_,_, _, _, #{session_tickets := disabled}) ->
    {ok, disabled};
session_tickets_tracker(ListenSocket, Lifetime, TicketStoreSize, MaxEarlyDataSize,
                        #{session_tickets := Mode,
                          anti_replay := AntiReplay,
                          stateless_tickets_seed := Seed}) ->
    tls_server_session_ticket_sup:start_child([ListenSocket, Mode, Lifetime,
                                               TicketStoreSize, MaxEarlyDataSize,
                                               AntiReplay, Seed]).

session_id_tracker(_, #{versions := [?TLS_1_3]}) ->
    {ok, not_relevant};
%% Regardless of the option reuse_sessions we need the session_id_tracker
%% to generate session ids, but no sessions will be stored unless
%% reuse_sessions = true.
session_id_tracker(_, #{erl_dist := true}) ->
    ssl_upgrade_server_session_cache_sup:start_child(dist);
session_id_tracker(ssl_unknown_listener, _) ->
    ssl_upgrade_server_session_cache_sup:start_child(normal);
session_id_tracker(ListenSocket, _) ->
    ssl_server_session_cache_sup:start_child(ListenSocket).
       
get_emulated_opts(TrackerPid) -> 
    call(TrackerPid, get_emulated_opts).
set_emulated_opts(TrackerPid, InetValues) -> 
    call(TrackerPid, {set_emulated_opts, InetValues}).
get_all_opts(TrackerPid) -> 
    call(TrackerPid, get_all_opts).

%%====================================================================
%% ssl_listen_tracker_sup API
%%====================================================================

start_link(Port, SockOpts, SslOpts) ->
    gen_server:start_link(?MODULE, [Port, SockOpts, SslOpts], []).

%%--------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.
%% Possible return values not used now. 
%% |  {ok, #state{}, timeout()} | ignore | {stop, term()}.		  
%%
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Listen, Opts, SslOpts]) ->
    process_flag(trap_exit, true),
    proc_lib:set_label({tls_listen_tracker, Listen}),
    Monitor = monitor_socket(Listen),
    {ok, #state{emulated_opts = do_set_emulated_opts(Opts, []), 
                listen_monitor = Monitor,
                ssl_opts = SslOpts}}.

%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), #state{}) -> {reply, Reply::term(), #state{}}.
%% Possible return values not used now.  
%%					      {reply, term(), #state{}, timeout()} |
%%					      {noreply, #state{}} |
%%					      {noreply, #state{}, timeout()} |
%%					      {stop, reason(), term(), #state{}} |
%%					      {stop, reason(), #state{}}.
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_emulated_opts, Opts0}, _From,
	    #state{emulated_opts = Opts1} = State) ->
    Opts = do_set_emulated_opts(Opts0, Opts1),
    {reply, ok, State#state{emulated_opts = Opts}};
handle_call(get_emulated_opts, _From,
	    #state{emulated_opts = Opts} = State) ->
    {reply, {ok, Opts}, State};
handle_call(get_all_opts, _From,
	    #state{emulated_opts = EmOpts,
		   ssl_opts = SslOpts} = State) ->
    {reply, {ok, EmOpts, SslOpts}, State}.

%%--------------------------------------------------------------------
-spec  handle_cast(term(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, State)-> 
    {noreply, State}.

%%--------------------------------------------------------------------
-spec handle_info(term(), #state{}) ->  {stop, ssl:reason(), #state{}}.
%% Possible return values not used now.
%%			              {noreply, #state{}}.
%%				      |{noreply, #state{}, timeout()} |
%%				     
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------
handle_info({'DOWN', Monitor, _, _, _}, #state{listen_monitor = Monitor} = State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
-spec terminate(ssl:reason(), #state{}) -> ok.
%%		       
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, list()) -> {ok, #state{}}.			 
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

start_tls_server_connection(SslOpts, Port, Socket, EmOpts, Trackers, CbInfo) ->
    Options = {SslOpts, EmOpts, Trackers},
    try tls_gen_connection:start_fsm(Port, Socket, Options, self(), CbInfo, infinity) of
        Result ->
            Result
    catch
        exit:{noproc, _} ->
            {error, ssl_not_started}
    end.

start_tls_client_connection(Host, Port, Socket, SslOpts, EmOpts, CbInfo, Timeout) ->
    Options = {SslOpts, EmOpts, undefined},
    try tls_gen_connection:start_fsm(Host, Port, Socket, Options, self(), CbInfo, Timeout) of
        Result ->
            Result
    catch
        exit:{noproc, _} ->
            {error, ssl_not_started}
    end.

split_options(Transport, Opts) ->
    split_options(Opts, emulated_options(Transport), [], []).

split_options([], _, SocketOpts, EmuOpts) ->
    {SocketOpts, EmuOpts};
split_options([{Name, _} = Opt | Opts], Emu, SocketOpts, EmuOpts) ->
    case lists:member(Name, Emu) of
	true ->
	    split_options(Opts, Emu, SocketOpts, [Opt | EmuOpts]);
	false ->
	    split_options(Opts, Emu, [Opt | SocketOpts], EmuOpts)
    end;
split_options([Name | Opts], Emu, SocketOptNames, EmuOptNames) ->
    case lists:member(Name, Emu) of
	true ->
	    split_options(Opts, Emu, SocketOptNames, [Name | EmuOptNames]);
	false ->
	    split_options(Opts, Emu, [Name | SocketOptNames], EmuOptNames)
    end.

do_set_emulated_opts([], Opts) ->
    Opts;
do_set_emulated_opts([{active, N0} | Rest], Opts) when is_integer(N0) ->
    N = update_active_n(N0, proplists:get_value(active, Opts, false)),
    do_set_emulated_opts(Rest, [{active, N} | proplists:delete(active, Opts)]);
do_set_emulated_opts([{Name,_} = Opt | Rest], Opts) ->
    do_set_emulated_opts(Rest, [Opt | proplists:delete(Name, Opts)]).

update_active_n(New, Current) ->
    if
        is_integer(Current), New + Current =< 0 ->
            false;
        is_integer(Current) ->
            New + Current;
        New =< 0 ->
            false;
        true ->
            New
    end.

get_socket_opts(_, [], _) ->
    [];
get_socket_opts(ListenSocket, SockOptNames, Cb) ->
    {ok, Opts} = Cb:getopts(ListenSocket, SockOptNames),
    Opts.

get_emulated_opts(TrackerPid, EmOptNames) -> 
    {ok, EmOpts} = get_emulated_opts(TrackerPid),
    lists:map(fun(Name) -> {value, Value} = lists:keysearch(Name, 1, EmOpts),
			   Value end,
	      EmOptNames).

emulated_socket_options(InetValues, #socket_options{
				       mode   = Mode,
				       header = Header,
				       active = Active,
				       packet = Packet,
				       packet_size = Size}) ->
    #socket_options{
       mode   = proplists:get_value(mode, InetValues, Mode),
       header = proplists:get_value(header, InetValues, Header),
       active = proplists:get_value(active, InetValues, Active),
       packet = proplists:get_value(packet, InetValues, Packet),
       packet_size = proplists:get_value(packet_size, InetValues, Size)
      }.

emulated_options([{mode, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(mode, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(mode, Emulated)]);
emulated_options([{header, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(header, Value),
    emulated_options(Opts, Inet,  [Opt | proplists:delete(header, Emulated)]);
emulated_options([{active, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(active, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(active, Emulated)]);
emulated_options([{packet, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(packet, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet, Emulated)]);
emulated_options([{packet_size, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(packet_size, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet_size, Emulated)]);
emulated_options([{high_watermark, Value} = Opt | Opts], Inet, Emulated) ->
    case lists:keymember(high_watermark, 1, Emulated) of
        true ->
            validate_inet_option(high_watermark, Value),
            emulated_options(Opts, Inet, [Opt | proplists:delete(high_watermark, Emulated)]);
        false ->
            emulated_options(Opts, [Opt|Inet], Emulated)
    end;
emulated_options([{low_watermark, Value} = Opt | Opts], Inet, Emulated) ->
    case lists:keymember(low_watermark, 1, Emulated) of
        true ->
            validate_inet_option(low_watermark, Value),
            emulated_options(Opts, Inet, [Opt | proplists:delete(low_watermark, Emulated)]);
        false ->
            emulated_options(Opts, [Opt|Inet], Emulated)
    end;
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {options, {mode,Value}}});
validate_inet_option(packet, Value)
  when not (is_atom(Value) orelse is_integer(Value)) ->
    throw({error, {options, {packet,Value}}});
validate_inet_option(packet_size, Value)
  when not is_integer(Value) ->
    throw({error, {options, {packet_size,Value}}});
validate_inet_option(header, Value)
  when not is_integer(Value) ->
    throw({error, {options, {header,Value}}});
validate_inet_option(active, Value)
  when Value >= -32768, Value =< 32767 ->
    ok;
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {options, {active,Value}}});
validate_inet_option(_, _) ->
    ok.

connect_error(Transport, Host, Port, UserOpts, Timeout) ->
    lists:flatten(io_lib:format("~p:connect(~p, ~p, ~p, ~p)",
                                [Transport, Host, Port, UserOpts, Timeout])).
