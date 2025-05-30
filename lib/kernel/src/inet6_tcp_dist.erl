%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-module(inet6_tcp_dist).
-moduledoc false.

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/2, accept/1, accept_connection/5,
         setup/5, close/1, select/1, address/0, is_node_name/1]).

-export([setopts/2, getopts/2]).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    inet_tcp_dist:gen_select(inet6_tcp, Node).

%% ------------------------------------------------------------
%%  Get address family
%%  address() => #net_address{}
%% ------------------------------------------------------------

address() ->
    inet_tcp_dist:gen_address(inet6_tcp).

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name, Host) ->
    inet_tcp_dist:gen_listen(inet6_tcp, Name, Host).

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    inet_tcp_dist:gen_accept(inet6_tcp, Listen).

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tcp_dist:gen_accept_connection(inet6_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    inet_tcp_dist:gen_setup(inet6_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

%%
%% Close a socket.
%%
close(Socket) ->
    inet6_tcp:close(Socket).
    
is_node_name(Node) when is_atom(Node) ->
    inet_tcp_dist:is_node_name(Node).

setopts(S, Opts) ->
    inet_tcp_dist:setopts(S, Opts).

getopts(S, Opts) ->
    inet_tcp_dist:getopts(S, Opts).
