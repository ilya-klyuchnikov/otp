%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxNavigationKeyEvent).
-moduledoc """
This event class contains information about navigation events, generated by navigation
keys such as tab and page down.

This event is mainly used by wxWidgets implementations. A `m:wxNavigationKeyEvent`
handler is automatically provided by wxWidgets when you enable keyboard navigation inside
a window by inheriting it from wxNavigationEnabled<>.

See: `wxWindow:navigate/2`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxNavigationKeyEvent](https://docs.wxwidgets.org/3.2/classwx_navigation_key_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxNavigationKeyEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getCurrentFocus/1,getDirection/1,isFromTab/1,isWindowChange/1,setCurrentFocus/2,
  setDirection/2,setFromTab/2,setWindowChange/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxNavigationKeyEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxNavigationKeyEventType() :: 'navigation_key'.
-export_type([wxNavigationKeyEvent/0, wxNavigationKey/0, wxNavigationKeyEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns true if the navigation was in the forward direction.".
-spec getDirection(This) -> boolean() when
	This::wxNavigationKeyEvent().
getDirection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_GetDirection),
  wxe_util:rec(?wxNavigationKeyEvent_GetDirection).

-doc "Sets the direction to forward if `direction` is true, or backward if false.".
-spec setDirection(This, Direction) -> 'ok' when
	This::wxNavigationKeyEvent(), Direction::boolean().
setDirection(#wx_ref{type=ThisT}=This,Direction)
 when is_boolean(Direction) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,Direction,?get_env(),?wxNavigationKeyEvent_SetDirection).

-doc """
Returns true if the navigation event represents a window change (for example, from
Ctrl-Page Down in a notebook).
""".
-spec isWindowChange(This) -> boolean() when
	This::wxNavigationKeyEvent().
isWindowChange(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_IsWindowChange),
  wxe_util:rec(?wxNavigationKeyEvent_IsWindowChange).

-doc "Marks the event as a window change event.".
-spec setWindowChange(This, WindowChange) -> 'ok' when
	This::wxNavigationKeyEvent(), WindowChange::boolean().
setWindowChange(#wx_ref{type=ThisT}=This,WindowChange)
 when is_boolean(WindowChange) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,WindowChange,?get_env(),?wxNavigationKeyEvent_SetWindowChange).

-doc """
Returns true if the navigation event was from a tab key.

This is required for proper navigation over radio buttons.
""".
-spec isFromTab(This) -> boolean() when
	This::wxNavigationKeyEvent().
isFromTab(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_IsFromTab),
  wxe_util:rec(?wxNavigationKeyEvent_IsFromTab).

-doc "Marks the navigation event as from a tab key.".
-spec setFromTab(This, FromTab) -> 'ok' when
	This::wxNavigationKeyEvent(), FromTab::boolean().
setFromTab(#wx_ref{type=ThisT}=This,FromTab)
 when is_boolean(FromTab) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,FromTab,?get_env(),?wxNavigationKeyEvent_SetFromTab).

-doc "Returns the child that has the focus, or NULL.".
-spec getCurrentFocus(This) -> wxWindow:wxWindow() when
	This::wxNavigationKeyEvent().
getCurrentFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_GetCurrentFocus),
  wxe_util:rec(?wxNavigationKeyEvent_GetCurrentFocus).

-doc "Sets the current focus window member.".
-spec setCurrentFocus(This, CurrentFocus) -> 'ok' when
	This::wxNavigationKeyEvent(), CurrentFocus::wxWindow:wxWindow().
setCurrentFocus(#wx_ref{type=ThisT}=This,#wx_ref{type=CurrentFocusT}=CurrentFocus) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  ?CLASS(CurrentFocusT,wxWindow),
  wxe_util:queue_cmd(This,CurrentFocus,?get_env(),?wxNavigationKeyEvent_SetCurrentFocus).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
