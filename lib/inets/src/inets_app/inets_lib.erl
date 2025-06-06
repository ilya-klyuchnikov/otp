%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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

-module(inets_lib).
-moduledoc false.

-export([millisec_passed/1, formated_timestamp/0, format_timestamp/1]).



millisec_passed(T0) ->
    erlang:convert_time_unit(erlang:monotonic_time() - T0,
			     native,
			     micro_seconds) div 1000.

%% Return formatted time stamp (e.g. 2015:03:16 10:05:23 1234)
formated_timestamp() ->
    format_timestamp( os:timestamp() ).

%% Return formatted time stamp (e.g. 2015:03:16 10:05:23 1234)
format_timestamp({_N1, _N2, N3} = Tme) ->
    {Date, Time}   = calendar:now_to_datetime(Tme),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).
