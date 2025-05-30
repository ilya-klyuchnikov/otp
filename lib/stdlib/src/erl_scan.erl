%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US        control
%% 040 - 057    SPC - /         punctuation
%% 060 - 071    0 - 9           digit
%% 072 - 100    : - @           punctuation
%% 101 - 132    A - Z           uppercase
%% 133 - 140    [ - `           punctuation
%% 141 - 172    a - z           lowercase
%% 173 - 176    { - ~           punctuation
%% 177          DEL             control
%% 200 - 237                    control
%% 240 - 277    NBSP - ¿        punctuation
%% 300 - 326    À - Ö           uppercase
%% 327          ×               punctuation
%% 330 - 336    Ø - Þ           uppercase
%% 337 - 366    ß - ö           lowercase
%% 367          ÷               punctuation
%% 370 - 377    ø - ÿ           lowercase
%%
%% Many punctuation characters have special meaning:
%%  $\s, $_, $", $$, $%, $', $.
%% DEL is a punctuation.
%%
%% Must watch using × \327, very close to x \170.

-module(erl_scan).
-moduledoc """
The Erlang token scanner.

This module contains functions for tokenizing (scanning) characters into Erlang
tokens.

## Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLocation, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

## Notes

The continuation of the first call to the re-entrant input functions must be
`[]`. For a complete description of how the re-entrant input scheme works, see
Armstrong, Virding and Williams: 'Concurrent Programming in Erlang', Chapter 13.

## See Also

`m:erl_anno`, `m:erl_parse`, `m:io`
""".

%%% External exports

-export([string/1,string/2,string/3,tokens/3,tokens/4,
         format_error/1,reserved_word/1,
         f_reserved_word/1]).

-export([column/1,end_location/1,line/1,location/1,text/1,
         category/1,symbol/1]).

%%% Private
-export([continuation_location/1]).

-export_type([error_info/0,
              options/0,
              return_cont/0,
              token/0, tokens/0,
              tokens_result/0]).

%% Removed functions and types
-removed([{set_attribute,3,"use erl_anno:set_line/2 instead"},
          {attributes_info,'_',
           "use erl_anno:{column,line,location,text}/1 instead"},
          {token_info,'_',
           "use erl_scan:{category,column,line,location,symbol,text}/1 instead"}]).

-removed_type([{column,0,"use erl_anno:column() instead"},
               {line,0,"use erl_anno:line() instead"},
               {location,0,"use erl_anno:location() instead"}]).

%%%
%%% Defines and type definitions
%%%

-define(COLUMN(C), (is_integer(C) andalso C >= 1)).
%% Line numbers less than zero have always been allowed:
-define(ALINE(L), is_integer(L)).
-define(STRING(S), is_list(S)).
-define(RESWORDFUN(F), is_function(F, 1)).

-type category() :: atom().
-type resword_fun() :: fun((atom()) -> boolean()).
-type text_fun() :: fun((atom(), string()) -> boolean()).
-type option() :: 'return' | 'return_white_spaces' | 'return_comments'
                | 'text' | {'reserved_word_fun', resword_fun()}
                | {'text_fun', text_fun()} | {'compiler_internal', [term()]}.
-type options() :: option() | [option()].
-type symbol() :: atom() | float() | integer() | string().
-type token() :: {category(), Anno :: erl_anno:anno(), symbol()}
               | {category(), Anno :: erl_anno:anno()}.
-type tokens() :: [token()].
-type error_description() :: term().
-type error_info() :: {erl_anno:location(), module(), error_description()}.

%%% Local record.
-record(erl_scan,
        {resword_fun = fun reserved_word/1    :: resword_fun(),
         text_fun    = fun(_, _) -> false end :: text_fun(),
         ws          = false                  :: boolean(),
         comment     = false                  :: boolean(),
         has_fun     = false                  :: boolean(),
         %% True if requested to parse %ssa%-check comments
         checks      = false                  :: boolean(),
         %% True if we're scanning inside a %ssa%-check comment
         in_check    = false                  :: boolean()}).

%%----------------------------------------------------------------------------

-doc """
Uses an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when an `ErrorInfo`
structure is processed (see section
[Error Information](`m:erl_scan#module-error-information`)).
""".
-spec format_error(ErrorDescriptor) -> string() when
      ErrorDescriptor :: error_description().
format_error({unterminated,char}) -> "unterminated character";
format_error({unterminated,What,Head}) ->
    %% The reported position should be the first char in Head
    lists:flatten(
      ["unterminated ", string_thing(What), " starting with ",
       io_lib:write_string(Head, string_quote(What))]);
format_error({illegal,Type}) ->
    lists:flatten(io_lib:fwrite("illegal ~w", [Type]));
format_error({base,Base}) ->
    lists:flatten(io_lib:fwrite("illegal base '~w'", [Base]));
format_error(indentation) ->
    "bad indentation in triple-quoted string";
format_error(white_space) ->
    "not white space after start of triple-quoted string";
format_error(string_concat) ->
    "adjacent string literals without intervening white space";
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

-doc(#{equiv => string(String, 1)}).
-spec string(String) -> Return when
      String :: string(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String) ->
    string(String, 1, []).

-doc(#{equiv => string(String, StartLocation, [])}).
-spec string(String, StartLocation) -> Return when
      String :: string(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      StartLocation :: erl_anno:location(),
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String, StartLocation) ->
    string(String, StartLocation, []).

-doc """
Takes the list of characters `String` and tries to scan (tokenize) them.

Returns one of the following:

- **`{ok, Tokens, EndLocation}`** - `Tokens` are the Erlang tokens from
  `String`. `EndLocation` is the first location after the last token.

- **`{error, ErrorInfo, ErrorLocation}`** - An error occurred. `ErrorLocation`
  is the first location after the erroneous token.

`StartLocation` indicates the initial location when scanning starts. If
`StartLocation` is a line, `Anno`, `EndLocation`, and `ErrorLocation` are lines.
If `StartLocation` is a pair of a line and a column, `Anno` takes the form of an
opaque compound data type, and `EndLocation` and `ErrorLocation` are pairs of a
line and a column. The _token annotations_ contain information about the column
and the line where the token begins, as well as the text of the token (if option
`text` is specified), all of which can be accessed by calling `column/1`,
`line/1`, `location/1`, and `text/1`.

A _token_ is a tuple containing information about syntactic category, the token
annotations, and the terminal symbol. For punctuation characters (such as `;`
and `|`) and reserved words, the category and the symbol coincide, and the token
is represented by a two-tuple. Three-tuples have one of the following forms:

- `{atom, Anno, atom()}`
- `{char, Anno, char()}`
- `{comment, Anno, string()}`
- `{float, Anno, float()}`
- `{integer, Anno, integer()}`
- `{var, Anno, atom()}`
- `{white_space, Anno, string()}`

Valid options:

- **`{reserved_word_fun, reserved_word_fun()}`** - A callback function that is
  called when the scanner has found an unquoted atom. If the function returns
  `true`, the unquoted atom itself becomes the category of the token. If the
  function returns `false`, `atom` becomes the category of the unquoted atom.

- **`return_comments`** - Return comment tokens.

- **`return_white_spaces`** - Return white space tokens. By convention, a
  newline character, if present, is always the first character of the text
  (there cannot be more than one newline in a white space token).

- **`return`** - Short for `[return_comments, return_white_spaces]`.

- **`text`{: #text }** - Include the token text in the token annotation. The
  text is the part of the input corresponding to the token. See also
  [`text_fun`](`m:erl_scan#text_fun`).

- **`{text_fun, text_fun()}`{: #text_fun }** - A callback function used to
  determine whether the full text for the token shall be included in the token
  annotation. Arguments of the function are the category of the token and the
  full token string. This is only used when [`text`](`m:erl_scan#text`) is not
  present. If neither are present the text will not be saved in the token
  annotation.

- **`{compiler_internal, term()}`{: #compiler_interal }** - Pass
  compiler-internal options to the scanner. The set of internal options
  understood by the scanner should be considered experimental and can thus be
  changed at any time without prior warning.

  The following options are currently understood:

  - **`ssa_checks`** - Tokenizes source code annotations used for encoding tests
    on the BEAM SSA code produced by the compiler.
""".
-spec string(String, StartLocation, Options) -> Return when
      String :: string(),
      Options :: options(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      StartLocation :: erl_anno:location(),
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String, Line, Options) when ?STRING(String), ?ALINE(Line) ->
    string1(String, options(Options), Line, no_col, []);
string(String, {Line,Column}, Options) when ?STRING(String),
                                            ?ALINE(Line),
                                            ?COLUMN(Column) ->
    string1(String, options(Options), Line, Column, []).

-type char_spec() :: string() | 'eof'.
-type cont_fun() :: fun((char_spec(), #erl_scan{},
                         erl_anno:line(), erl_anno:column(),
                         tokens(), any()) -> any()).
-opaque return_cont() :: {erl_scan_continuation,
                          string(), erl_anno:column(), tokens(),
                          erl_anno:line(),
                          #erl_scan{}, any(), cont_fun()}.
-type tokens_result() :: {'ok', Tokens :: tokens(),
                          EndLocation :: erl_anno:location()}
                       | {'eof', EndLocation :: erl_anno:location()}
                       | {'error', ErrorInfo :: error_info(),
                          EndLocation :: erl_anno:location()}.

-doc(#{equiv => tokens(Continuation, CharSpec, StartLocation, [])}).
-spec tokens(Continuation, CharSpec, StartLocation) -> Return when
      Continuation :: return_cont() | [],
      CharSpec :: char_spec(),
      StartLocation :: erl_anno:location(),
      Return :: {'done',Result :: tokens_result(),LeftOverChars :: char_spec()}
              | {'more', Continuation1 :: return_cont()}.
tokens(Cont, CharSpec, StartLocation) ->
    tokens(Cont, CharSpec, StartLocation, []).

-doc """
This is the re-entrant scanner, which scans characters until either a _dot_ ('.'
followed by a white space) or `eof` is reached.

It returns:

- **`{done, Result, LeftOverChars}`** - Indicates that there is sufficient input
  data to get a result. `Result` is:

  - **`{ok, Tokens, EndLocation}`** - The scanning was successful. `Tokens` is
    the list of tokens including _dot_.

  - **`{eof, EndLocation}`** - End of file was encountered before any more
    tokens.

  - **`{error, ErrorInfo, EndLocation}`** - An error occurred. `LeftOverChars`
    is the remaining characters of the input data, starting from `EndLocation`.

- **`{more, Continuation1}`** - More data is required for building a term.
  `Continuation1` must be passed in a new call to `tokens/3,4` when more data is
  available.

The `CharSpec` `eof` signals end of file. `LeftOverChars` then takes the value
`eof` as well.

For a description of the options, see `string/3`.
""".
-spec tokens(Continuation, CharSpec, StartLocation, Options) -> Return when
      Continuation :: return_cont() | [],
      CharSpec :: char_spec(),
      StartLocation :: erl_anno:location(),
      Options :: options(),
      Return :: {'done',Result :: tokens_result(),LeftOverChars :: char_spec()}
              | {'more', Continuation1 :: return_cont()}.
tokens([], CharSpec, Line, Options) when ?ALINE(Line) ->
    tokens1(CharSpec, options(Options), Line, no_col, [], fun scan/6, []);
tokens([], CharSpec, {Line,Column}, Options) when ?ALINE(Line),
                                                  ?COLUMN(Column) ->
    tokens1(CharSpec, options(Options), Line, Column, [], fun scan/6, []);
tokens({erl_scan_continuation,Cs,Col,Toks,Line,St,Any,Fun},
       CharSpec, _Loc, _Opts) ->
    tokens1(Cs++CharSpec, St, Line, Col, Toks, Fun, Any).

-doc false.
continuation_location({erl_scan_continuation,_,no_col,_,Line,_,_,_}) ->
    Line;
continuation_location({erl_scan_continuation,_,Col,_,Line,_,_,_}) ->
    {Line,Col}.

-doc "Returns the column of `Token`'s collection of annotations.".
-doc(#{since => <<"OTP 18.0">>}).
-spec column(Token) -> erl_anno:column() | 'undefined' when
      Token :: token().

column(Token) ->
    erl_anno:column(element(2, Token)).

-doc """
Returns the end location of the text of `Token`'s collection of annotations. If
there is no text, `undefined` is returned.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec end_location(Token) -> erl_anno:location() | 'undefined' when
      Token :: token().

end_location(Token) ->
    erl_anno:end_location(element(2, Token)).

-doc "Returns the line of `Token`'s collection of annotations.".
-doc(#{since => <<"OTP 18.0">>}).
-spec line(Token) -> erl_anno:line() when
      Token :: token().

line(Token) ->
    erl_anno:line(element(2, Token)).

-doc "Returns the location of `Token`'s collection of annotations.".
-doc(#{since => <<"OTP 18.0">>}).
-spec location(Token) -> erl_anno:location() when
      Token :: token().

location(Token) ->
    erl_anno:location(element(2, Token)).

-doc """
Returns the text of `Token`'s collection of annotations. If there is no text,
`undefined` is returned.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec text(Token) -> erl_anno:text() | 'undefined' when
      Token :: token().

text(Token) ->
    erl_anno:text(element(2, Token)).

-doc "Returns the category of `Token`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec category(Token) -> category() when
      Token :: token().

category({Category,_Anno}) ->
    Category;
category({Category,_Anno,_Symbol}) ->
    Category;
category(T) ->
    erlang:error(badarg, [T]).

-doc "Returns the symbol of `Token`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec symbol(Token) -> symbol() when
      Token :: token().

symbol({Category,_Anno}) ->
    Category;
symbol({_Category,_Anno,Symbol}) ->
    Symbol;
symbol(T) ->
    erlang:error(badarg, [T]).

%%%
%%% Local functions
%%%

string_thing(What) ->
    case What of
        atom ->
            "atom";
        string ->
            "string";
        {string,_N} ->
            "triple-quoted string";
        {sigil,Name,Q1,Q2} ->
            [$~,atom_to_list(Name),Q1,Q2," sigil string"];
        {sigil,Name,_N} ->
            "triple-quoted " ++ [$~,atom_to_list(Name) | " sigil string"]
    end.

string_quote(atom) -> $'; %'
string_quote(_)    -> $". %"

-define(WHITE_SPACE(C),
        (is_integer(C) andalso
         (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240))).
-define(DIGIT(C), (is_integer(C) andalso $0 =< C andalso C =< $9)).
-define(NAMECHAR(C),
        (is_integer(C) andalso
         (C >= $a andalso C =< $z orelse
          C >= $A andalso C =< $Z orelse
          C =:= $_ orelse
          C >= $0 andalso C =< $9 orelse
          C =:= $@ orelse
          C >= $ß andalso C =< $ÿ andalso C =/= $÷ andalso
          C >= $À andalso C =< $Þ andalso C =/= $×))).
-define(CHAR(C), (is_integer(C) andalso 0 =< C andalso C < 16#110000)).
-define(UNICODE(C),
        (is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF))).

-define(UNI255(C), (is_integer(C) andalso 0 =< C andalso C =< 16#ff)).

-define(NO_SIGIL, {}).

options(Opts0) when is_list(Opts0) ->
    Opts = lists:foldr(fun expand_opt/2, [], Opts0),
    [RW_fun] =
        case opts(Opts, [reserved_word_fun], []) of
            badarg ->
                erlang:error(badarg, [Opts0]);
            R ->
                R
        end,
    Comment = proplists:get_bool(return_comments, Opts),
    WS = proplists:get_bool(return_white_spaces, Opts),
    Txt = proplists:get_bool(text, Opts),
    TxtFunOpt = proplists:get_value(text_fun, Opts, none),
    Internal = proplists:get_value(compiler_internal, Opts, []),
    Checks = proplists:get_bool(ssa_checks, Internal),
    DefTxtFun = fun(_, _) -> Txt end,
    {HasFun, TxtFun} =
        if
            Txt -> {Txt, DefTxtFun};
            TxtFunOpt == none -> {Txt, DefTxtFun};
            true -> {true, TxtFunOpt}
        end,
    #erl_scan{resword_fun = RW_fun,
              comment     = Comment,
              ws          = WS,
              text_fun    = TxtFun,
              has_fun     = HasFun,
              checks      = Checks};
options(Opt) ->
    options([Opt]).

opts(Options, [Key|Keys], L) ->
    V = case lists:keyfind(Key, 1, Options) of
            {reserved_word_fun,F} when ?RESWORDFUN(F) ->
                {ok,F};
            {Key,_} ->
                badarg;
            false ->
                {ok,default_option(Key)}
        end,
    case V of
        badarg ->
            badarg;
        {ok,Value} ->
            opts(Options, Keys, [Value|L])
    end;
opts(_Options, [], L) ->
    lists:reverse(L).

default_option(reserved_word_fun) ->
    fun reserved_word/1.

expand_opt(return, Os) ->
    [return_comments,return_white_spaces|Os];
expand_opt(O, Os) ->
    [O|Os].

tokens1(Cs, St, Line, Col, Toks, Fun, Any) when ?STRING(Cs); Cs =:= eof ->
    case Fun(Cs, St, Line, Col, Toks, Any) of
        {more,{Cs0,Nst,Ncol,Ntoks,Nline,Nany,Nfun}} ->
            {more,{erl_scan_continuation,Cs0,Ncol,Ntoks,Nline,Nst,Nany,Nfun}};
        {ok,Toks0,eof,Nline,Ncol} ->
            Res = case Toks0 of
                      [] ->
                          {eof,location(Nline, Ncol)};
                      _ ->
                          {ok,lists:reverse(Toks0),location(Nline,Ncol)}
                  end,
            {done,Res,eof};
        {ok,Toks0,Rest,Nline,Ncol} ->
            {done,{ok,lists:reverse(Toks0),location(Nline, Ncol)},Rest};
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

string1(Cs, St, Line, Col, Toks) ->
    case scan1(Cs, St, Line, Col, Toks) of
        {more,{Cs0,Nst,Ncol,Ntoks,Nline,Any,Fun}} ->
            case Fun(Cs0++eof, Nst, Nline, Ncol, Ntoks, Any) of
                {ok,Toks1,_Rest,Line2,Col2} ->
                    {ok,lists:reverse(Toks1),location(Line2, Col2)};
                {{error,_,_}=Error,_Rest} ->
                    Error
            end;
        {ok,Ntoks,[_|_]=Rest,Nline,Ncol} ->
            string1(Rest, St, Nline, Ncol, Ntoks);
        {ok,Ntoks,_,Nline,Ncol} ->
            {ok,lists:reverse(Ntoks),location(Nline, Ncol)};
        {{error,_,_}=Error,_Rest} ->
            Error
    end.


scan(Cs, #erl_scan{}=St, Line, Col, Toks, _) ->
    scan1(Cs, St, Line, Col, Toks).

scan1([$\s|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_spcs(Cs, St, Line, Col, Toks, 1);
scan1([$\s|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([$\n|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_newline(Cs, St, Line, Col, Toks);
scan1([$\n|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
%% Optimization: some very common punctuation characters:
scan1([$,|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ",", ',', 1);
scan1([$(|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "(", '(', 1);
scan1([$)|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ")", ')', 1);
scan1([${|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "{", '{', 1);
scan1([$}|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "}", '}', 1);
scan1([$[|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "[", '[', 1);
scan1([$]|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "]", ']', 1);
scan1([$;|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ";", ';', 1);
scan1([$_=C|Cs], St, Line, Col, Toks) ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([$\%=C|Cs], St, Line, Col, Toks) when St#erl_scan.checks ->
    scan_check(Cs, St, Line, Col, Toks, [C]);
scan1([$\%|Cs], St, Line, Col, Toks) when not St#erl_scan.comment ->
    skip_comment(Cs, St, Line, Col, Toks, 1);
scan1([$\%=C|Cs], St, Line, Col, Toks) ->
    scan_comment(Cs, St, Line, Col, Toks, [C]);
%% More punctuation characters below.
scan1([C|_], _St, _Line, _Col0, _Toks) when not ?CHAR(C) ->
    error({not_character,C});
scan1([C|Cs], St, Line, Col, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C], no_underscore);
scan1("..."++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "...", '...', 3);
scan1(".."=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1(".."++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "..", '..', 2);
scan1("."=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1("&&"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "&&", '&&', 2);
scan1("&"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1("&"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "&", '&', 1);
scan1([$.=C|Cs], St, Line, Col, Toks) ->
    scan_dot(Cs, St, Line, Col, Toks, [C]);
scan1([$'|Cs], St, Line, Col, Toks) -> %' Emacs
    scan_qatom(Cs, St, Line, Col, Toks);
scan1([$"|_]=Cs, St, Line, Col, Toks) -> %" Emacs
    scan_string(Cs, St, Line, Col, Toks, ?NO_SIGIL);
scan1([$~=C|Cs], St, Line, Col, Toks) ->
    scan_sigil_prefix(Cs, St, Line, Col, Toks, [C]);
scan1([$$|Cs], St, Line, Col, Toks) ->
    scan_char(Cs, St, Line, Col, Toks);
scan1([$\r|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    white_space_end(Cs, St, Line, Col, Toks, 1, "\r");
scan1([C|Cs], St, Line, Col, Toks) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([$\t|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_tabs(Cs, St, Line, Col, Toks, 1);
scan1([$\t|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([C|Cs], St, Line, Col, Toks) when ?WHITE_SPACE(C) ->
    case St#erl_scan.ws of
        true ->
            scan_white_space(Cs, St, Line, Col, Toks, [C]);
        false ->
            skip_white_space(Cs, St, Line, Col, Toks, 1)
    end;
%% Punctuation characters and operators, first recognise multiples.
%% ?= for the maybe ... else ... end construct
scan1("?="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "?=", '?=', 2);
scan1("?"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% << <:- <- <:= <=
scan1("<<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<<", '<<', 2);
scan1("<:-"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<:-", '<:-', 3);
scan1("<-"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<-", '<-', 2);
scan1("<:="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<:=", '<:=', 3);
scan1("<="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<=", '<=', 2);
scan1("<:"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1("<"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% >> >=
scan1(">>"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">>", '>>', 2);
scan1(">="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">=", '>=', 2);
scan1(">"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% -> --
scan1("->"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "->", '->', 2);
scan1("--"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "--", '--', 2);
scan1("-"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% ++
scan1("++"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "++", '++', 2);
scan1("+"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% =:= =/= =< == =>
scan1("=:="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=:=", '=:=', 3);
scan1("=:"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1("=/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=/=", '=/=', 3);
scan1("=/"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1("=<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=<", '=<', 2);
scan1("=>"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=>", '=>', 2);
scan1("=="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "==", '==', 2);
scan1("="=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% /=
scan1("/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "/=", '/=', 2);
scan1("/"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% ||
scan1("||"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "||", '||', 2);
scan1("|"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% :=
scan1(":="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ":=", ':=', 2);
%% :: for typed records
scan1("::"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "::", '::', 2);
scan1(":"=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
%% Optimization: punctuation characters less than 127:
scan1([$=|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=", '=', 1);
scan1([$:|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ":", ':', 1);
scan1([$||Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "|", '|', 1);
scan1([$#|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "#", '#', 1);
scan1([$/|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "/", '/', 1);
scan1([$?|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "?", '?', 1);
scan1([$-|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "-", '-', 1);
scan1([$+|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "+", '+', 1);
scan1([$*|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "*", '*', 1);
scan1([$<|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<", '<', 1);
scan1([$>|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">", '>', 1);
scan1([$!|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "!", '!', 1);
scan1([$@|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "@", '@', 1);
scan1([$\\|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "\\", '\\', 1);
scan1([$^|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "^", '^', 1);
scan1([$`|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "`", '`', 1);
scan1([$~|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "~", '~', 1);
%% End of optimization.
scan1([C|Cs], St, Line, Col, Toks) when ?UNI255(C) ->
    Str = [C],
    tok2(Cs, St, Line, Col, Toks, Str, list_to_atom(Str), 1);
scan1([C|Cs], _St, Line, Col, _Toks) when ?CHAR(C) ->
    Ncol = incr_column(Col, 1),
    scan_error({illegal,character}, Line, Col, Line, Ncol, Cs);
scan1([]=Cs, St, Line, Col, Toks) ->
    {more,{Cs,St,Col,Toks,Line,[],fun scan/6}};
scan1(eof=Cs, _St, Line, Col, Toks) ->
    {ok,Toks,Cs,Line,Col}.

scan_atom_fun(Cs, #erl_scan{}=St, Line, Col, Toks, Ncs) ->
    scan_atom(Cs, St, Line, Col, Toks, Ncs).

scan_atom(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more,Ncs} ->
            {more,{[],St,Col,Toks,Line,Ncs,fun scan_atom_fun/6}};
        {WcsR,Cs} ->
            Wcs = lists:reverse(WcsR),
            try list_to_atom(Wcs) of
                Name ->
                    case (St#erl_scan.resword_fun)(Name) of
                        true ->
                            tok2(Cs, St, Line, Col, Toks, Wcs, Name);
                        false ->
                            tok3(Cs, St, Line, Col, Toks, atom, Wcs, Name)
                    end
            catch
                _:_ ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,atom}, Line, Col, Line, Ncol, Cs)
            end
    end.


scan_variable_fun(Cs, #erl_scan{}=St, Line, Col, Toks, Ncs) ->
    scan_variable(Cs, St, Line, Col, Toks, Ncs).

scan_variable(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more,Ncs} ->
            {more,{[],St,Col,Toks,Line,Ncs,fun scan_variable_fun/6}};
        {WcsR,Cs} ->
            Wcs = lists:reverse(WcsR),
            try list_to_atom(Wcs) of
                Name ->
                    tok3(Cs, St, Line, Col, Toks, var, Wcs, Name)
            catch
                _:_ ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,var}, Line, Col, Line, Ncol, Cs)
            end
    end.


scan_name([C|_]=Cs, Wcs) when not ?CHAR(C) ->
    {Wcs,Cs};
scan_name([C|Cs], Wcs) when C >= $a, C =< $z ->
    scan_name(Cs, [C|Wcs]);
scan_name([C|Cs], Wcs) when C >= $A, C =< $Z ->
    scan_name(Cs, [C|Wcs]);
scan_name([$_=C|Cs], Wcs) ->
    scan_name(Cs, [C|Wcs]);
scan_name([C|Cs], Wcs) when ?DIGIT(C) ->
    scan_name(Cs, [C|Wcs]);
scan_name([$@=C|Cs], Wcs) ->
    scan_name(Cs, [C|Wcs]);
scan_name([C|Cs], Wcs) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_name(Cs, [C|Wcs]);
scan_name([C|Cs], Wcs) when C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, [C|Wcs]);
scan_name([], Wcs) ->
    {more,Wcs};
scan_name(Cs, Wcs) ->
    {Wcs,Cs}.


-define(STR(Cl, St, S),
        case ((St)#erl_scan.has_fun)
            andalso ((St)#erl_scan.text_fun)((Cl), begin S end) of
            true  -> begin S end;
            false -> []
        end).
-define(STR(Cl, St, Tmp, S),
        case ((St)#erl_scan.has_fun) of
            true ->
                Tmp = begin S end,
                case ((St)#erl_scan.text_fun)((Cl), Tmp) of
                    true ->  Tmp;
                    false -> []
                end;
            false -> []
        end).

scan_dot([C|_]=Cs, St, Line, Col, Toks, Ncs)
  when St#erl_scan.in_check, C =/= $. ->
    tok2(Cs, St#erl_scan{in_check=false}, Line, Col, Toks, Ncs, '.', 1);
scan_dot([$%|_]=Cs, St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, ?STR(dot, St, Ncs)),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot([$\n=C|Cs], St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, ?STR(dot, St, Ncs++[C])),
    {ok,[{dot,Anno}|Toks],Cs,Line+1,new_column(Col, 1)};
scan_dot([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    Anno = anno(Line, Col, St, ?STR(dot, St, Ncs++[C])),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 2)};
scan_dot(eof=Cs, St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, ?STR(dot, St, Ncs)),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot(Cs, St, Line, Col, Toks, Ncs) ->
    tok2(Cs, St, Line, Col, Toks, Ncs, '.', 1).

%%% White space characters are very common, so it is worthwhile to
%%% scan them fast and store them compactly. (The words "whitespace"
%%% and "white space" usually mean the same thing. The Erlang
%%% specification denotes the characters with ASCII code in the
%%% interval 0 to 32 as "white space".)
%%%
%%% Convention: if there is a white newline ($\n) it will always be
%%% the first character in the text string. As a consequence, there
%%% cannot be more than one newline in a white_space token string.
%%%
%%% Some common combinations are recognized, some are not. Examples
%%% of the latter are tab(s) followed by space(s), like "\t  ".
%%% (They will be represented by two (or more) tokens.)
%%%
%%% Note: the character sequence "\r\n" is *not* recognized since it
%%% would violate the property that $\n will always be the first
%%% character. (But since "\r\n\r\n" is common, it pays off to
%%% recognize "\n\r".)

scan_newline([$\s|Cs], St, Line, Col, Toks) ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\t|Cs], St, Line, Col, Toks) ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\r|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\r");
scan_newline([$\f|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\f");
scan_newline([], St, Line, Col, Toks) ->
    {more,{[$\n],St,Col,Toks,Line,[],fun scan/6}};
scan_newline(Cs, St, Line, Col, Toks) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, "\n").

scan_nl_spcs_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N)
  when is_integer(N) ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, N).

scan_nl_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 17 ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, N+1);
scan_nl_spcs([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun scan_nl_spcs_fun/6}};
scan_nl_spcs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_spcs(N)).

scan_nl_tabs_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N)
  when is_integer(N) ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, N).

scan_nl_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 11 ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, N+1);
scan_nl_tabs([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun scan_nl_tabs_fun/6}};
scan_nl_tabs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_tabs(N)).

scan_nl_white_space_fun(Cs, #erl_scan{}=St, Line, Col, Toks, Ncs) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, Ncs).

%% Note: returning {more,Cont} is meaningless here; one could just as
%% well return several tokens. But since tokens() scans up to a full
%% stop anyway, nothing is gained by not collecting all white spaces.
scan_nl_white_space([$\n|Cs], #erl_scan{has_fun = false}=St, Line, no_col=Col,
                    Toks0, Ncs) ->
    Toks = [{white_space,anno(Line),lists:reverse(Ncs)}|Toks0],
    scan_newline(Cs, St, Line+1, Col, Toks);
scan_nl_white_space([$\n|Cs], St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    Anno = anno(Line, Col, St, ?STR(white_space, St, Ncs)),
    Token = {white_space,Anno,Ncs},
    scan_newline(Cs, St, Line+1, new_column(Col, length(Ncs)), [Token|Toks]);
scan_nl_white_space([C|Cs], St, Line, Col, Toks, Ncs)
  when ?WHITE_SPACE(C) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_nl_white_space([]=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_nl_white_space_fun/6}};
scan_nl_white_space(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col,
                    Toks, Ncs) ->
    Anno = anno(Line),
    scan1(Cs, St, Line+1, Col, [{white_space,Anno,lists:reverse(Ncs)}|Toks]);
scan_nl_white_space(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    Anno = anno(Line, Col, St, ?STR(white_space, St, Ncs)),
    Token = {white_space,Anno,Ncs},
    scan1(Cs, St, Line+1, new_column(Col, length(Ncs)), [Token|Toks]).

newline_end(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col,
            Toks, _N, Ncs) ->
    scan1(Cs, St, Line+1, Col, [{white_space,anno(Line),Ncs}|Toks]);
newline_end(Cs, #erl_scan{}=St, Line, Col, Toks, N, Ncs) ->
    Anno = anno(Line, Col, St, ?STR(white_space, St, Ncs)),
    scan1(Cs, St, Line+1, new_column(Col, N), [{white_space,Anno,Ncs}|Toks]).

scan_spcs_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N)
  when is_integer(N), N >= 1 ->
    scan_spcs(Cs, St, Line, Col, Toks, N).

scan_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 16 ->
    scan_spcs(Cs, St, Line, Col, Toks, N+1);
scan_spcs([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun scan_spcs_fun/6}};
scan_spcs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, spcs(N)).

scan_tabs_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N)
  when is_integer(N), N >= 1 ->
    scan_tabs(Cs, St, Line, Col, Toks, N).

scan_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 10 ->
    scan_tabs(Cs, St, Line, Col, Toks, N+1);
scan_tabs([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun scan_tabs_fun/6}};
scan_tabs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, tabs(N)).

skip_white_space_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N) ->
    skip_white_space(Cs, St, Line, Col, Toks, N).

skip_white_space([$\n|Cs], St, Line, Col, Toks, _N) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
skip_white_space([C|Cs], St, Line, Col, Toks, N) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Line, Col, Toks, N+1);
skip_white_space([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun skip_white_space_fun/6}};
skip_white_space(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

scan_white_space_fun(Cs, #erl_scan{}=St, Line, Col, Toks, Ncs) ->
    scan_white_space(Cs, St, Line, Col, Toks, Ncs).

%% Maybe \t and \s should break the loop.
scan_white_space([$\n|_]=Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs));
scan_white_space([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    scan_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_white_space([]=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_white_space_fun/6}};
scan_white_space(Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs)).

-compile({inline,[white_space_end/7]}).

white_space_end(Cs, St, Line, Col, Toks, N, Ncs) ->
    tok3(Cs, St, Line, Col, Toks, white_space, Ncs, Ncs, N).

scan_char([$\\|Cs]=Cs0, St, Line, Col, Toks) ->
    case scan_escape(Cs, incr_column(Col, 2)) of
        more ->
            {more,{[$$|Cs0],St,Col,Toks,Line,[],fun scan/6}};
        {error,Ncs,Error,Ncol} ->
            scan_error(Error, Line, Col, Line, Ncol, Ncs);
        {eof,Ncol} ->
            scan_error({unterminated,char}, Line, Col, Line, Ncol, eof);
        {nl,Val,Str,Ncs,Ncol} ->
            Anno = anno(Line, Col, St, ?STR(char, St, "$\\"++Str)), %"
            Ntoks = [{char,Anno,Val}|Toks],
            scan1(Ncs, St, Line+1, Ncol, Ntoks);
        {Val,Str,Ncs,Ncol} ->
            Anno = anno(Line, Col, St, ?STR(char, St, "$\\"++Str)), %"
            Ntoks = [{char,Anno,Val}|Toks],
            scan1(Ncs, St, Line, Ncol, Ntoks)
    end;
scan_char([$\n=C|Cs], St, Line, Col, Toks) ->
    Anno = anno(Line, Col, St, ?STR(char, St, [$$,C])),
    scan1(Cs, St, Line+1, new_column(Col, 1), [{char,Anno,C}|Toks]);
scan_char([C|Cs], St, Line, Col, Toks) when ?UNICODE(C) ->
    Anno = anno(Line, Col, St, ?STR(char, St, [$$,C])),
    scan1(Cs, St, Line, incr_column(Col, 2), [{char,Anno,C}|Toks]);
scan_char([C|_Cs], _St, Line, Col, _Toks) when ?CHAR(C) ->
    scan_error({illegal,character}, Line, Col, Line, incr_column(Col, 1), eof);
scan_char([], St, Line, Col, Toks) ->
    {more,{[$$],St,Col,Toks,Line,[],fun scan/6}};
scan_char(eof, _St, Line, Col, _Toks) ->
    scan_error({unterminated,char}, Line, Col, Line, incr_column(Col, 1), eof).

%% Sigil Prefix is scanned here and handled in scan_tqstring/6
%% and scan_qstring/8 where non-verbatim sigils (that handle
%% character escape sequences) are enumerated.
%% Search for SigilType or sigil_type.
%%
%% The bogus SigilType = {} is used to indicate a regular
%% non-sigil string to scan_qstring/8 and scan_sigil_suffix/6.
%%
%% Sigils are also handled in erl_parse:build_sigil/3 that
%% enumerates all sigils, cause an error for unknown ones,
%% and transforms known ones.
%%
scan_sigil_prefix(Cs, St, Line, Col, Toks, Wcs) ->
    case scan_name(Cs, Wcs) of
        {more, Nwcs} ->
            {more, {[],St,Col,Toks,Line,Nwcs,fun scan_sigil_prefix/6}};
        {Nwcs,Ncs} ->
            Type = sigil_prefix,
            Ncol = incr_column(Col, length(Nwcs)),
            SigilCs = lists:reverse(Nwcs),
            try list_to_atom(tl(SigilCs)) of
                SigilType when is_atom(SigilType) ->
                    Anno = anno(Line, Col, St, ?STR(Type, St, SigilCs)),
                    Tok = {Type,Anno,SigilType},
                    scan_string(Ncs, St, Line, Ncol, [Tok|Toks], SigilType)
            catch _ : _ ->
                    scan_error({illegal,Type}, Line, Col, Line, Ncol, Ncs)
            end
    end.

%% Assuming that a string starts with a delimiter right here
scan_string(Cs, St, Line, Col, Toks, SigilType) ->
    case Cs of
        [$",$",$"|Ncs] -> %"
            scan_tqstring(Ncs, St, Line, Col, Toks, SigilType, 3);
        [$",$"] ->
            {more,{Cs,St,Col,Toks,Line,SigilType,fun scan_string/6}};
        [$"] ->
            {more,{Cs,St,Col,Toks,Line,SigilType,fun scan_string/6}};
        [Q1|Ncs] ->
            case string_right_delimiter(Q1) of
                undefined ->
                    scan_error({illegal,string}, Line, Col, Line, Col, Cs);
                Q2 when is_integer(Q2) ->
                    scan_qstring(Ncs, St, Line, Col, Toks, SigilType, Q1, Q2)
            end;
        [] ->
            {more,{Cs,St,Col,Toks,Line,SigilType,fun scan_string/6}};
        eof ->
            scan_error({illegal,string}, Line, Col, Line, Col, Cs)
    end.

%% String delimiters are enumerated here.
%%
%% The $" and $' delimiters are also handled in scan1/5 above
%% to recognize regular strings and quoted atoms.
%% For triple-quoted strings it is hardcoded in scan_string/6
%% and scan_tqstring_lines/8 to look for $".
%%
-compile({inline,[string_right_delimiter/1]}).
string_right_delimiter(C) ->
    case C of
        $( -> $);
        $[ -> $];
        ${ -> $};
        $< -> $>;
        _ when
              C =:= $/;
              C =:= $|;
              C =:= $#;
              C =:= $`;
              C =:= $'; %'
              C =:= $"  %"
              ->
            C;
        _ ->
            undefined
    end.

-record(tqs, % Triple-quoted String state
        {line,                  % Line number of first quote character
         col,                   % Column number     - " -
         sigil_type,            % atom() | {}
         qs,                    % Number of quote characters in delimiter
         verbatim,              % Ignore escape sequences?
         qn = undefined,        % Quote character counter | undefined
         str = "",              % Scanned text (reversed)
         content_r = [],        % Reverse list of reversed content lines
         acc = ""}).            % Current line accumulator
%%
%% Triple-quoted string (delimited by at least 3 double quote characters)
%%
%% Start delimiter may be followed only by white space.
%%
%% End delimiter may be preceeded only by white space, which defines
%% the indentation to strip from all content lines.
%%
scan_tqstring(Cs, St, Line, Col, Toks, SigilType, Qs) ->
    scan_tqstring(Cs, St, Line, Col, Toks, {SigilType,Qs}).
%%
%% Scan leading $" characters until we have them all, then scan lines
scan_tqstring(Cs, St, Line, Col, Toks, {SigilType,Qs}) ->
    case scan_count(Cs, $", Qs) of %"
        {[],Nqs} ->
            {more,
             {[], St, Col, Toks, Line, {SigilType,Nqs},
              fun scan_tqstring/6}};
        {Ncs,Nqs} ->
            Verbatim =
                if
                    SigilType =:= 'b';      % binary(), non-verbatim
                    SigilType =:= 's' ->    % string(), non-verbatim
                        false;
                    true ->                 % The rest are verbatim
                        true
                end,
            Tqs =
                #tqs{
                   line = Line, col = Col,
                   sigil_type = SigilType, qs = Nqs, verbatim = Verbatim,
                   str = lists_duplicate(Nqs, $", "") }, %"
            scan_tqstring_lines(Ncs, St, Line, int_column(Col)+Qs, Toks, Tqs)
    end.

%% Scan off characters that are C and count them
scan_count([C|Cs], C, N) ->  scan_count(Cs, C, N + 1);
scan_count(Cs, _, N)     ->  {Cs,N}.

scan_tqstring_lines(Cs, St, Line, Col, Toks, Tqs) ->
    #tqs{ qn = Qn, str = Str, content_r = ContentR, acc = Acc } = Tqs,
    case
        scan_tqstring_lines(
          Cs, Tqs, Line, int_column(Col), Str, Qn, ContentR, Acc)
    of
        {ok,Ncs,Nline,Ncol,Nstr,NcontentR,IndentR} ->
            %% Last line - post process the content
            scan_tqstring_finish(
              Ncs, St, Nline, Ncol, Toks,
              Tqs#tqs{ str = Nstr, content_r = NcontentR, acc = IndentR });
        {more,Ncs,Nline,Ncol,Nstr,Nqn,NcontentR,Nacc} ->
            {more,
             {Ncs, St, Ncol, Toks, Nline,
              Tqs#tqs{
                qn = Nqn, str = Nstr, content_r = NcontentR, acc = Nacc },
              fun scan_tqstring_lines/6}};
        {error,Ncs,Nline,Ncol,Nline_1,Ncol_1,Error} ->
            Col0 = Tqs#tqs.col,
            scan_error(
              Error, Nline, new_column(Col0, Ncol),
              Nline_1, new_column(Col0, Ncol_1), Ncs)
    end.

%% Inner loop that minimizes garbage creation
scan_tqstring_lines(Cs0, Tqs, Line, Col, Str, Qn, ContentR, Acc) ->
    case Cs0 of
        [$\n=C|Cs] ->
            %% New line, restart searching for ending delimiter
            scan_tqstring_lines(
              Cs, Tqs, Line+1, 1,
              [C|Str], Tqs#tqs.qs, [[C|Acc]|ContentR], []);
        [C|Cs] ->
            if
                C =:= $", is_integer(Qn) -> %"
                    %% Possibly part of ending delimiter
                    Nstr = [C|Str],
                    Nacc = [C|Acc],
                    Ncol = Col + 1,
                    if
                        Qn =:= 1 ->
                            %% Complete ending delimiter
                            IndentR = lists:nthtail(Tqs#tqs.qs, Nacc),
                            {ok,Cs,Line,Col+1,Nstr,ContentR,IndentR};
                        true ->
                            %% Collect and count this quote char
                            scan_tqstring_lines(
                              Cs, Tqs, Line, Ncol,
                              Nstr, Qn-1, ContentR, Nacc)
                    end;
                ?WHITE_SPACE(C), is_integer(Qn) ->
                    %% White space while searching for ending delimiter
                    Nstr = [C|Str],
                    Nacc = [C|Acc],
                    Ncol = Col + 1,
                    if
                        Qn =:= Tqs#tqs.qs ->
                            %% White space before first quote char
                            %% - just collect
                            scan_tqstring_lines(
                              Cs, Tqs, Line, Ncol,
                              Nstr, Qn, ContentR, Nacc);
                        true ->
                            %% White space after too few end quote chars
                            %% - stop searching for end quote chars
                            scan_tqstring_lines(
                              Cs, Tqs, Line, Ncol,
                              Nstr, undefined, ContentR, Nacc)
                    end;
                C =:= $\\, not Tqs#tqs.verbatim ->
                    case scan_escape(Cs, Col) of
                        more ->
                            {more,Cs,Line,Col,Str,Qn,ContentR,Acc};
                        {error,Ncs,Error,Ncol} ->
                            {error,Ncs,Line,Ncol,Line,Ncol+1,Error};
                        {eof,Ncol} ->
                            scan_tqstring_eof(
                              [Acc|ContentR], Tqs, Line, Ncol+1);
                        {nl,Val,ValStr,Ncs,Ncol} ->
                            %% An escaped newline: "\\\n",
                            %% counts as just a newline: "\n",
                            %% see [$\n|_] above
                            Nstr = lists:reverse(ValStr, [C|Str]),
                            scan_tqstring_lines(
                              Ncs, Tqs, Line+1, Ncol,
                              Nstr, Tqs#tqs.qs, [[Val|Acc]|ContentR], []);
                        {Val,ValStr,Ncs,Ncol} ->
                            Nstr = lists:reverse(ValStr, [C|Str]),
                            scan_tqstring_lines(
                              Ncs, Tqs, Line, Ncol+1,
                              Nstr, undefined, ContentR, [Val|Acc])
                    end;
                ?UNICODE(C) ->
                    %% Not searching / stop searching for ending delimiter
                    scan_tqstring_lines(
                      Cs, Tqs, Line, Col+1,
                      [C|Str], undefined, ContentR, [C|Acc]);
                ?CHAR(C) ->
                    %% Illegal Unicode character
                    {error,Cs,Line,Col,Line,Col+1,{illegal,character}}
            end;
        [] ->
            {more,Cs0,Line,Col,Str,Qn,ContentR,Acc};
        eof ->
            scan_tqstring_eof([Acc|ContentR], Tqs, Line, Col)
    end.

scan_tqstring_eof(ContentR, Tqs, Line, Col) ->
    #tqs{ line = Line0, col = Col0, sigil_type = SigilType, qs = Qs } = Tqs,
    {error,eof,Line0,incr_column(Col0, Qs),Line,Col,
     {unterminated,
      if
          SigilType =:= ?NO_SIGIL ->
              {string,Qs};
          is_atom(SigilType) ->
              {sigil,SigilType,Qs}
      end,
      string_head(ContentR)}}.

%% Strip last line newline,
%% get indentation definition from last line,
%% strip indentation from content lines,
%% check white space on first line after start delimiter,
%% create the string token,
%% done
%%
scan_tqstring_finish(Cs, St, Line, Col, Toks, Tqs) ->
    %% IndentR :: Indentation characters, reversed
    %%
    #tqs{
       line = Line0, col = Col0,
       content_r = ContentR, acc = IndentR } = Tqs,
    NcontentR = strip_last_line_newline_r(ContentR),
    %%
    %% NcontentR is now the string's content lines
    %% including the characters after the opening quote sequence
    %% (the 0:th line?), in reversed order, each line reversed.
    %% Newline has been stripped from the last content line,
    %% all others has newline characters as from the input.
    case
        tqstring_finish(lists:reverse(IndentR), NcontentR, Line-1)
    of
        Content when is_list(Content) ->
            #tqs{ str = Str, sigil_type = SigilType } = Tqs,
            AnnoStr = ?STR(string, St, Text, lists:reverse(Str)),
            Tok = {string,anno(Line0, Col0, St, AnnoStr),Content},
            scan_sigil_suffix(
              Cs, St, Line, new_column(Col0, Col), [Tok|Toks], SigilType);
        {Tag=indentation, ErrorLine, ErrorCol} ->
            scan_error(
              Tag, ErrorLine, new_column(Col0, ErrorCol),
              Line, new_column(Col0, Col), Cs);
        {Tag=white_space, N} ->
            scan_error(
              Tag, Line0, incr_column(Col0, Tqs#tqs.qs+N),
              Line, new_column(Col0, Col), Cs)
    end.

%% Strip newline from the last line, but not if it is the only line
%%
strip_last_line_newline_r(ContentR=[_]) ->
    ContentR;
strip_last_line_newline_r([LastLineR|ContentR]) ->
    [strip_newline_r(LastLineR)|ContentR].

strip_newline_r("\n\r"++Rcs) -> Rcs;
strip_newline_r("\n"++Rcs) -> Rcs.

%% Loop from last to first line and remember the last error,
%% so the last error that is found will be the one reported,
%% that is: the first in the string.
%%
%% Build the string content one line at the time by first
%% prepending the line to Content and then spripping
%% the defined indentation.
%%
%% For the first (0:th) line, strip the newline and then
%% check that it contains only white space
%%
tqstring_finish(Indent, ContentR, Line) ->
    tqstring_finish(Indent, ContentR, Line, undefined, "").
%%
tqstring_finish(_Indent, [FirstLineR], _Line, Error, Content) ->
    NfirstLineR = strip_newline_r(FirstLineR),
    FirstLine = lists:reverse(NfirstLineR),
    %% First line; check that it is all white space
    case check_white_space(FirstLine) of
        ok ->
            if
                Error =:= undefined ->
                    Content;
                true ->
                    Error
            end;
        N ->
            {white_space, N}
    end;
tqstring_finish(
  Indent, [StringR|StringsR], Line, Error, Content) ->
    case strip_indent(Indent, lists:reverse(StringR, Content)) of
        Ncontent when is_list(Ncontent) ->
            tqstring_finish(Indent, StringsR, Line-1, Error, Ncontent);
        ErrorCol when is_integer(ErrorCol) ->
            Nerror = {indentation, Line, ErrorCol},
            tqstring_finish(Indent, StringsR, Line-1, Nerror, "")
    end.

%% Strip the defined indentation from the string content
%%
strip_indent(Indent, Cs) ->
    case Cs of
        %% Allow empty content lines to have no indentation
        "\r\n"++_ -> Cs;
        "\n"++_   -> Cs;
        ""        -> Cs; % The last newline is stripped
        _ ->
            strip_indent(Indent, Cs, 1)
    end.
%%
strip_indent([C|Indent], [C|Cs], Col) ->
    strip_indent(Indent, Cs, Col+1);    % Strip
strip_indent([], Cs, _) -> Cs;          % Done
strip_indent(_, _, Col) -> Col.         % Incorrect indentation

%% Check that all characters are white space and return 'ok',
%% or return the number of white space characters
check_white_space(Cs) ->
    check_white_space(Cs, 0).
%%
check_white_space([], _) ->
    ok;
check_white_space([C|Cs], N) ->
    if
        ?WHITE_SPACE(C) ->
            check_white_space(Cs, N+1);
        true ->
            N
    end.


-record(qstring,
        { line, col, sigil_type, q1, q2, str = undefined, wcs = "" }).
%%
%% Quoted string
%%
scan_qstring(Cs, St, Line, Col, Toks, SigilType, Q1, Q2) ->
    if
        SigilType =:= ?NO_SIGIL;
        SigilType =:= '';       % The vanilla (default) sigil
        SigilType =:= 'b';      % binary() with escape sequences
        SigilType =:= 's' ->    % string() with escape sequences
            Ncol = incr_column(Col, 1), % Quote character
            scan_qstring(
              Cs, St, Line, Ncol, Toks,
              #qstring{
                 line = Line, col = Col,
                 sigil_type = SigilType, q1 = Q1, q2 = Q2 });
        true ->                 % Verbatim string
            scan_vstring(Cs, St, Line, Col, Toks, SigilType, Q1, Q2)
    end.

scan_qstring(
  Cs, #erl_scan{}=St, Line, Col, Toks,
  #qstring{ q2 = Q2, str = Str, wcs = Wcs } = Qstring) ->
    case scan_string0(Cs, St, Line, Col, Q2, Str, Wcs) of
        {ok, {Ncs,Nline,Ncol,Nstr,Nwcs}} ->
            #qstring{
               line = Line0, col = Col0,
               sigil_type = SigilType, q1 = Q1 } = Qstring,
            AnnoStr =
                ?STR(string, St, Text, [Q1|lists:reverse(Nstr, [Q2])]),
            Anno = anno(Line0, Col0, St, AnnoStr),
            Tok = {string,Anno,lists:reverse(Nwcs)},
            scan_sigil_suffix(Ncs, St, Nline, Ncol, [Tok|Toks], SigilType);
        {more, {Ncs,Nline,Ncol,Nstr,Nwcs}} ->
            Nqstring = Qstring#qstring{ str = Nstr, wcs = Nwcs },
            {more, {Ncs,St,Ncol,Toks,Nline,Nqstring,fun scan_qstring/6}};
        {eof, {Ncs,Nline,Ncol,Nwcs}} ->
            #qstring{
               line = Line0, col = Col0,
               sigil_type = SigilType, q1 = Q1 } = Qstring,
            scan_error(
              {unterminated,
               if
                   SigilType =:= ?NO_SIGIL ->
                       string;
                   is_atom(SigilType) ->
                       {sigil,SigilType,Q1,Q2}
               end,
               string_head([Nwcs])},
              Line0, incr_column(Col0, 1), Nline, Ncol, Ncs);
        {{error,_,_}, _Ncs} = Error ->
            Error
    end.

scan_sigil_suffix(Cs, St, Line, Col, Toks, ?NO_SIGIL) ->
    scan_string_concat(Cs, St, Line, Col, Toks, "");
scan_sigil_suffix(Cs, St, Line, Col, Toks, SigilType)
  when is_atom(SigilType) -> % Sigil string - scan suffix
    scan_sigil_suffix(Cs, St, Line, Col, Toks, "");
%%
scan_sigil_suffix(Cs, St, Line, Col, Toks, Wcs) when is_list(Wcs) ->
    case scan_name(Cs, Wcs) of
        {more, Nwcs} ->
            {more, {[],St,Col,Toks,Line,Nwcs,fun scan_sigil_suffix/6}};
        {Nwcs,Ncs} ->
            Type = sigil_suffix,
            Ncol = incr_column(Col, length(Nwcs)),
            Suffix = lists:reverse(Nwcs),
            try list_to_atom(Suffix) of
                A when is_atom(A) ->
                    Anno = anno(Line, Col, St, ?STR(Type, St, Suffix)),
                    Tok = {Type,Anno,Suffix},
                    scan_string_concat(
                      Ncs, St, Line, Ncol, [Tok|Toks], Suffix)
            catch _ : _ ->
                    scan_error({illegal,Type}, Line, Col, Line, Ncol, Ncs)
            end
    end.

scan_string_concat(Cs, St, Line, Col, Toks, "" = SigilSuffix) ->
    case Cs of
        [$"|_] ->
            scan_error(string_concat, Line, Col, Line, Col, Cs);
        [] ->
            {more,
             {Cs,St,Col,Toks,Line,SigilSuffix,fun scan_string_concat/6}};
        _ ->
            scan1(Cs, St, Line, Col, Toks)
    end;
scan_string_concat(Cs, St, Line, Col, Toks, SigilSuffix)
  when is_list(SigilSuffix) ->
    scan1(Cs, St, Line, Col, Toks).


-record(vstring,
        { line, col, sigil_type, q1, q2, wcs = "" }).
%%
%% Verbatim quoted string (not triple-quoted)
%%
scan_vstring(Cs, St, Line, Col, Toks, SigilType, Q1, Q2) ->
    Vstring =
        #vstring{
           line = Line, col = Col,
           sigil_type = SigilType, q1 = Q1, q2 = Q2 },
    scan_vstring(Cs, St, Line, incr_column(Col, 1), Toks, Vstring).

scan_vstring(
  Cs, #erl_scan{}=St, Line, Col, Toks,
  #vstring{ q2 = Q2, wcs = Wcs } = Vstring) ->
    case scan_vstring(Cs, Q2, Line, Col, Wcs) of
        {ok, {Ncs,Nline,Ncol,Nwcs}} ->
            #vstring{
               line = Line0, col = Col0,
               sigil_type = SigilType, q1 = Q1 } = Vstring,
            AnnoStr =
                ?STR(string, St, Text, [Q1|lists:reverse(Nwcs, [Q2])]),
            Anno = anno(Line0, Col0, St, AnnoStr),
            Tok = {string,Anno,lists:reverse(Nwcs)},
            scan_sigil_suffix(Ncs, St, Nline, Ncol, [Tok|Toks], SigilType);
        {more, {Ncs,Nline,Ncol,Nwcs}} ->
            Nvstring = Vstring#vstring{ wcs = Nwcs },
            {more, {Ncs,St,Ncol,Toks,Nline,Nvstring,fun scan_vstring/6}};
        {eof, {Ncs,Nline,Ncol,Nwcs}} ->
            #vstring{
               line = Line0, col = Col0,
               sigil_type = SigilType, q1 = Q1 } = Vstring,
            scan_error(
              {unterminated,
               if
                   SigilType =:= ?NO_SIGIL ->
                       string;
                   is_atom(SigilType) ->
                       {sigil,SigilType,Q1,Q2}
               end,
               string_head([Nwcs])},
              Line0, incr_column(Col0, 1), Nline, Ncol, Ncs);
        {{error,_,_}, _Ncs} = Error ->
            Error
    end.

scan_vstring(Cs, Q, Line, no_col, Wcs) ->
    scan_vstring(Cs, Q, Line, Wcs);
scan_vstring(Cs, Q, Line, Col, Wcs) ->
    case Cs of
        [Q|Ncs] ->
            {ok, {Ncs,Line,Col+1,Wcs}};
        [$\n=C|Ncs] ->
            scan_vstring(Ncs, Q, Line+1, 1, [C|Wcs]);
        [C|Ncs] when ?UNICODE(C) ->
            scan_vstring(Ncs, Q, Line, Col+1, [C|Wcs]);
        [C|Ncs] when ?CHAR(C) ->
            scan_error(
              {illegal,character}, Line, Col, Line, Col+1, Ncs);
        [] ->
            {more, {Cs,Line,Col,Wcs}};
        eof ->
            {eof, {Cs,Line,Col,Wcs}}
    end.
%%
%% Duplicated code that optimizes for Col = no_col, which avoids
%% both testing for no_col before every incrementation,
%% and the incrementation itself
%%
scan_vstring(Cs, Q, Line, Wcs) ->
    case Cs of
        [Q|Ncs] ->
            {ok, {Ncs,Line,no_col,Wcs}};
        [$\n=C|Ncs] ->
            scan_vstring(Ncs, Q, Line+1, [C|Wcs]);
        [C|Ncs] when ?UNICODE(C) ->
            scan_vstring(Ncs, Q, Line, [C|Wcs]);
        [C|Ncs] when ?CHAR(C) ->
            Col = no_col,
            scan_error(
              {illegal,character}, Line, Col, Line, Col, Ncs);
        [] ->
            {more, {Cs,Line,no_col,Wcs}};
        eof ->
            {eof, {Cs,Line,no_col,Wcs}}
    end.


-record(qatom, { line, col, str = undefined, wcs = "" }).

scan_qatom(Cs, St, Line, Col, Toks) ->
    Qatom = #qatom{ line = Line, col = Col },
    scan_qatom(Cs, St, Line, incr_column(Col, 1), Toks, Qatom).

scan_qatom(
  Cs, #erl_scan{}=St, Line, Col, Toks,
  #qatom{ str = Str, wcs = Wcs } = Qatom) ->
    C = $', %'
    case scan_string0(Cs, St, Line, Col, C, Str, Wcs) of
        {ok, {Ncs,Nline,Ncol,Nstr,Nwcs}} ->
            #qatom{ line = Line0, col = Col0 } = Qatom,
            try list_to_atom(lists:reverse(Nwcs)) of
                A when is_atom(A) ->
                    AnnoStr =
                        ?STR(atom, St, Text, [C|lists:reverse(Nstr, [C])]),
                    Anno = anno(Line0, Col0, St, AnnoStr),
                    Tok = {atom,Anno,A},
                    scan1(Ncs, St, Nline, Ncol, [Tok|Toks])
            catch _ : _ ->
                    scan_error(
                      {illegal,atom}, Line0, Col0, Nline, Ncol, Ncs)
            end;
        {more, {Ncs,Nline,Ncol,Nstr,Nwcs}} ->
            Nqatom = Qatom#qatom{ str = Nstr, wcs = Nwcs },
            {more, {Ncs,St,Ncol,Toks,Nline,Nqatom,fun scan_qatom/6}};
        {eof, {Ncs,Nline,Ncol,Nwcs}} ->
            #qatom{ line = Line0, col = Col0 } = Qatom,
            scan_error(
              {unterminated,atom,string_head([Nwcs])},
              Line0, incr_column(Col0, 1), Nline, Ncol, Ncs);
        {{error,_,_}, _Ncs} = Error ->
            Error
    end.

string_head(ContentR) ->
    string:slice(lists_foldl_reverse(ContentR, ""), 0, 16).


scan_string0(Cs, #erl_scan{has_fun=true}, Line, Col, Q, Str, Wcs)
  when Str =/= undefined ->
    scan_string1(Cs, Line, Col, Q, Str, Wcs);
scan_string0(Cs, #erl_scan{}, Line, Col, Q, _Str, Wcs) ->
    if
        Col =:= no_col ->
            scan_string_no_col(Cs, Line, Q, Wcs);
        true ->
            scan_string_col(Cs, Line, Col, Q, Wcs)
    end.

%% Optimization. Col =:= no_col, non-escaped characters
scan_string_no_col([Q|Cs], Line, Q, Wcs) ->
    {ok, {Cs,Line,no_col,Wcs,Wcs}};
scan_string_no_col([$\n=C|Cs], Line, Q, Wcs) ->
    scan_string_no_col(Cs, Line+1, Q, [C|Wcs]);
scan_string_no_col([C|Cs], Line, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_no_col(Cs, Line, Q, [C|Wcs]);
scan_string_no_col(Cs, Line, Q, Wcs) ->
    scan_string1(Cs, Line, no_col, Q, Wcs, Wcs).

%% Optimization. Col =/= no_col, non-escaped characters
scan_string_col([Q|Cs], Line, Col, Q, Wcs) ->
    {ok, {Cs,Line,Col+1,Wcs,Wcs}};
scan_string_col([$\n=C|Cs], Line, _Col, Q, Wcs) ->
    scan_string_col(Cs, Line+1, 1, Q, [C|Wcs]);
scan_string_col([C|Cs], Line, Col, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_col(Cs, Line, Col+1, Q, [C|Wcs]);
scan_string_col(Cs, Line, Col, Q, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Wcs, Wcs).

%% Note: in those cases when a 'char_error' tuple is returned below it
%% is tempting to skip over characters up to the first Q character,
%% but then the end location of the error tuple would not correspond
%% to the start location of the returned Rest string. (Maybe the end
%% location could be modified, but that too is ugly.)
scan_string1([Q|Cs], Line, Col, Q, Str, Wcs) ->
    {ok, {Cs,Line,incr_column(Col, 1),Str,Wcs}};
scan_string1([$\n=C|Cs], Line, Col, Q, Str, Wcs) ->
    Ncol = new_column(Col, 1),
    scan_string1(Cs, Line+1, Ncol, Q, [C|Str], [C|Wcs]);
scan_string1([$\\|Cs]=Cs0, Line, Col, Q, Str, Wcs) ->
    case scan_escape(Cs, Col) of
        more ->
            {more, {Cs0,Line,Col,Str,Wcs}};
        {error,Ncs,Error,Ncol} ->
            scan_error(Error, Line, Col, Line, incr_column(Ncol, 1), Ncs);
        {eof=Ncs,Ncol} ->
            {eof, {Ncs,Line,incr_column(Ncol, 1),lists:reverse(Wcs)}};
        {nl,Val,ValStr,Ncs,Ncol} ->
            Nstr = lists:reverse(ValStr, [$\\|Str]),
            Nwcs = [Val|Wcs],
            scan_string1(Ncs, Line+1, Ncol, Q, Nstr, Nwcs);
        {Val,ValStr,Ncs,Ncol} ->
            Nstr = lists:reverse(ValStr, [$\\|Str]),
            Nwcs = [Val|Wcs],
            scan_string1(Ncs, Line, incr_column(Ncol, 1), Q, Nstr, Nwcs)
    end;
scan_string1([C|Cs], Line, no_col=Col, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, Line, Col, Q, [C|Str], [C|Wcs]);
scan_string1([C|Cs], Line, Col, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, Line, Col+1, Q, [C|Str], [C|Wcs]);
scan_string1([C|Cs], Line, Col, _Q, _Str, _Wcs) when ?CHAR(C) ->
    scan_error({illegal,character}, Line, Col, Line, incr_column(Col, 1), Cs);
scan_string1([]=Cs, Line, Col, _Q, Str, Wcs) ->
    {more, {Cs,Line,Col,Str,Wcs}};
scan_string1(eof=Cs, Line, Col, _Q, _Str, Wcs) ->
    {eof, {Cs,Line,Col,Wcs}}.

-define(OCT(C), (is_integer(C) andalso $0 =< C andalso C =< $7)).
-define(HEX(C), (is_integer(C) andalso
                 (C >= $0 andalso C =< $9 orelse
                  C >= $A andalso C =< $F orelse
                  C >= $a andalso C =< $f))).

%% \<1-3> octal digits
scan_escape([O1,O2,O3|Cs], Col) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,[O1,O2,O3],Cs,incr_column(Col, 3)};
scan_escape([O1,O2], _Col) when ?OCT(O1), ?OCT(O2) ->
    more;
scan_escape([O1,O2|Cs], Col) when ?OCT(O1), ?OCT(O2) ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,[O1,O2],Cs,incr_column(Col, 2)};
scan_escape([O1], _Col) when ?OCT(O1) ->
    more;
scan_escape([O1|Cs], Col) when ?OCT(O1) ->
    {O1 - $0,[O1],Cs,incr_column(Col, 1)};
%% \x{<hex digits>}
scan_escape([$x,${|Cs], Col) ->
    scan_hex(Cs, incr_column(Col, 2), []);
scan_escape([$x], _Col) ->
    more;
scan_escape([$x|eof], Col) ->
    {eof,incr_column(Col, 1)};
%% \x<2> hexadecimal digits
scan_escape([$x,H1,H2|Cs], Col) when ?HEX(H1), ?HEX(H2) ->
    Val = erlang:list_to_integer([H1,H2], 16),
    {Val,[$x,H1,H2],Cs,incr_column(Col, 3)};
scan_escape([$x,H1], _Col) when ?HEX(H1) ->
    more;
scan_escape([$x|Cs], Col) ->
    {error,Cs,{illegal,character},incr_column(Col, 1)};
%% \^X -> Ctrl+X
scan_escape([$^=C0,C|Cs], Col) when ?CHAR(C) ->
    case caret_char_code(C) of
        error ->
            {error,[C|Cs],{illegal,character},incr_column(Col, 1)};
        Code ->
            {Code,[C0,C],Cs,incr_column(Col, 2)}
    end;
scan_escape([$^], _Col) ->
    more;
scan_escape([$^|eof], Col) ->
    {eof,incr_column(Col, 1)};
scan_escape([$\n=C|Cs], Col) ->
    {nl,C,[C],Cs,new_column(Col, 1)};
scan_escape([C0|Cs], Col) when ?UNICODE(C0) ->
    C = escape_char(C0),
    {C,[C0],Cs,incr_column(Col, 1)};
scan_escape([C|Cs], Col) when ?CHAR(C) ->
    {error,Cs,{illegal,character},incr_column(Col, 1)};
scan_escape([], _Col) ->
    more;
scan_escape(eof, Col) ->
    {eof,Col}.

scan_hex([C|Cs], Col, Wcs) when ?HEX(C) ->
    scan_hex(Cs, incr_column(Col, 1), [C|Wcs]);
scan_hex(Cs, Col, Wcs) ->
    scan_hex_end(Cs, Col, Wcs, "x{").

scan_hex_end([$}|Cs], Col, [], _Str) ->
    %% Empty escape sequence.
    {error,Cs,{illegal,character},incr_column(Col, 1)};
scan_hex_end([$}|Cs], Col, Wcs0, Str0) ->
    Wcs = lists:reverse(Wcs0),
    try list_to_integer(Wcs, 16) of
        Val when ?UNICODE(Val) ->
            {Val,Str0++Wcs++[$}],Cs,incr_column(Col, 1)};
        _Val ->
            {error,Cs,{illegal,character},incr_column(Col, 1)}
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            {error,Cs,{illegal,character},incr_column(Col, 1)}
    end;
scan_hex_end([], _Col, _Wcs, _Str0) ->
    more;
scan_hex_end(eof, Col, _Wcs, _Str0) ->
    {eof,Col};
scan_hex_end(Cs, Col, _Wcs, _Str0) ->
    {error,Cs,{illegal,character},Col}.

escape_char($n) -> $\n;                         % \n = LF
escape_char($r) -> $\r;                         % \r = CR
escape_char($t) -> $\t;                         % \t = TAB
escape_char($v) -> $\v;                         % \v = VT
escape_char($b) -> $\b;                         % \b = BS
escape_char($f) -> $\f;                         % \f = FF
escape_char($e) -> $\e;                         % \e = ESC
escape_char($s) -> $\s;                         % \s = SPC
escape_char($d) -> $\d;                         % \d = DEL
escape_char(C) -> C.

caret_char_code($?) -> 16#7f;
caret_char_code(C) when $@ =< C, C =< $_; $a =< C, C =< $z -> C band 16#1f;
caret_char_code(_) -> error.

scan_number(Cs, #erl_scan{}=St, Line, Col, Toks, {Ncs, Us}) ->
    scan_number(Cs, St, Line, Col, Toks, Ncs, Us).

scan_number([C|Cs], St, Line, Col, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C|Ncs], Us);
scan_number([$_,Next|Cs], St, Line, Col, Toks, [Prev|_]=Ncs, _Us) when
      ?DIGIT(Next) andalso ?DIGIT(Prev) ->
    scan_number(Cs, St, Line, Col, Toks, [Next,$_|Ncs], with_underscore);
scan_number([$_]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_number/6}};
scan_number([$.,C|Cs], St, Line, Col, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C,$.|Ncs], Us);
scan_number([$.,C|_]=Cs0, _St, Line, Col, _Toks, Ncs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,float}, Line, Col, Line, incr_column(Col, length(Ncs)), Cs0);
scan_number([$.]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_number/6}};
scan_number([$#|Cs]=Cs0, St, Line, Col, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_integer(remove_digit_separators(Ncs, Us)) of
        B when is_integer(B), 2 =< B, B =< 1+$Z-$A+10 ->
            Bcs = Ncs++[$#],
            scan_based_num(Cs, St, Line, Col, Toks, B, [], Bcs, no_underscore);
        B when is_integer(B) ->
            Len = length(Ncs),
            scan_error({base,B}, Line, Col, Line, incr_column(Col, Len), Cs0)
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            scan_error({illegal,base}, Line, Col, Line, Col, Cs0)
    end;
scan_number([C|_]=Cs0, _St, Line, Col, _Toks, Ncs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,integer}, Line, Col, Line, incr_column(Col, length(Ncs)), Cs0);
scan_number([]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_number/6}};
scan_number(Cs, St, Line, Col, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_integer(remove_digit_separators(Ncs, Us), 10) of
        N ->
            tok3(Cs, St, Line, Col, Toks, integer, Ncs, N)
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,integer}, Line, Col, Line, Ncol, Cs)
    end.

remove_digit_separators(Number, no_underscore) ->
    Number;
remove_digit_separators(Number, with_underscore) ->
    [C || C <- Number, C =/= $_].

-define(BASED_DIGIT(C, B),
        (is_integer(C)
         andalso
           ((?DIGIT(C) andalso C < $0 + B)
            orelse (C >= $A andalso B > 10 andalso C < $A + B - 10)
            orelse (C >= $a andalso B > 10 andalso C < $a + B - 10)))).

scan_based_num(Cs, #erl_scan{}=St, Line, Col, Toks, {B,NCs,BCs,Us})
  when is_integer(B), 2 =< B, B =< 1+$Z-$A+10 ->
    scan_based_num(Cs, St, Line, Col, Toks, B, NCs, BCs, Us).

scan_based_num([C|Cs], St, Line, Col, Toks, B, Ncs, Bcs, Us) when
      ?BASED_DIGIT(C, B) ->
    scan_based_num(Cs, St, Line, Col, Toks, B, [C|Ncs], Bcs, Us);
scan_based_num([$_,Next|Cs], St, Line, Col, Toks, B, [Prev|_]=Ncs, Bcs, _Us)
      when ?BASED_DIGIT(Next, B) andalso ?BASED_DIGIT(Prev, B) ->
    scan_based_num(Cs, St, Line, Col, Toks, B, [Next,$_|Ncs], Bcs,
                   with_underscore);
scan_based_num([$_]=Cs, St, Line, Col, Toks, B, NCs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,NCs,BCs,Us},fun scan_based_num/6}};
scan_based_num([$.,C|Cs], St, Line, Col, Toks, B, Ncs, BCs, Us) when ?BASED_DIGIT(C, B) ->
    scan_based_fraction(Cs, St, Line, Col, Toks, B, [C,$.|Ncs], BCs, Us);
scan_based_num([$.,C|_]=Cs0, _St, Line, Col, _Toks, _B, Ncs, Bcs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,float}, Line, Col, Line, incr_column(Col, length(Ncs) + length(Bcs)), Cs0);
scan_based_num([$.]=Cs, St, Line, Col, Toks, B, Ncs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,Us},fun scan_based_num/6}};
scan_based_num([C|_]=Cs0, _St, Line, Col, _Toks, _B, Ncs, Bcs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,integer}, Line, Col, Line, incr_column(Col, length(Ncs) + length(Bcs)), Cs0);
scan_based_num([]=Cs, St, Line, Col, Toks, B, NCs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,NCs,BCs,Us},fun scan_based_num/6}};
scan_based_num(Cs, _St, Line, Col, _Toks, _B, [], Bcs, _Us) ->
    %% No actual digits following the base.
    Len = length(Bcs),
    Ncol = incr_column(Col, Len),
    scan_error({illegal,integer}, Line, Col, Line, Ncol, Cs);
scan_based_num(Cs, St, Line, Col, Toks, B, Ncs0, [_|_]=Bcs, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_integer(remove_digit_separators(Ncs, Us), B) of
        N ->
            tok3(Cs, St, Line, Col, Toks, integer, Bcs++Ncs, N)
    catch
        error:system_limit ->
            %% Extremely unlikely to occur in practice.
            Len = length(Bcs)+length(Ncs),
            Ncol = incr_column(Col, Len),
            scan_error({illegal,integer}, Line, Col, Line, Ncol, Cs)
    end.

scan_based_fraction(Cs, #erl_scan{}=St, Line, Col, Toks, {B,Ncs,BCs,Us}) ->
    scan_based_fraction(Cs, St, Line, Col, Toks, B, Ncs, BCs, Us).

scan_based_fraction([C|Cs], St, Line, Col, Toks, B, Ncs, BCs, Us) when ?BASED_DIGIT(C, B) ->
    scan_based_fraction(Cs, St, Line, Col, Toks, B, [C|Ncs], BCs, Us);
scan_based_fraction([$_,Next|Cs], St, Line, Col, Toks, B, [Prev|_]=Ncs, BCs, _Us) when
      ?BASED_DIGIT(Next, B) andalso ?BASED_DIGIT(Prev, B) ->
    scan_based_fraction(Cs, St, Line, Col, Toks, B, [Next,$_|Ncs], BCs, with_underscore);
scan_based_fraction([$_]=Cs, St, Line, Col, Toks, B, Ncs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,Us},fun scan_based_fraction/6}};
scan_based_fraction([$#,E|Cs], St, Line, Col, Toks, B, Ncs, BCs, Us) when E =:= $e; E =:= $E ->
    scan_based_exponent_sign(Cs, St, Line, Col, Toks, B, Ncs, BCs, [E,$#], Us);
scan_based_fraction([$#]=Cs, St, Line, Col, Toks, B, Ncs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,Us},fun scan_based_fraction/6}};
scan_based_fraction([C|_]=Cs0, _St, Line, Col, _Toks, _B, Ncs, BCs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,float}, Line, Col, Line, incr_column(Col, length(Ncs) + length(BCs)), Cs0);
scan_based_fraction([]=Cs, St, Line, Col, Toks, B, Ncs, BCs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,Us},fun scan_based_fraction/6}};
scan_based_fraction(Cs, St, Line, Col, Toks, B, Ncs, BCs, Us) ->
    based_float_end(Cs, St, Line, Col, Toks, B, Ncs, BCs, [], Us).

scan_based_exponent_sign(Cs, #erl_scan{}=St, Line, Col, Toks, {B,Ncs,BCs,ECs,Us}) ->
    scan_based_exponent_sign(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us).

scan_based_exponent_sign([C|Cs], St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) when
      C =:= $+; C =:= $- ->
    scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, [C|ECs], Us);
scan_based_exponent_sign([]=Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs, ECs,Us},fun scan_based_exponent_sign/6}};
scan_based_exponent_sign(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) ->
    scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us).

scan_based_exponent(Cs, #erl_scan{}=St, Line, Col, Toks, {B,Ncs,BCs,ECs,Us}) ->
    scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us).

scan_based_exponent([C|Cs], St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) when ?DIGIT(C) ->
    scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, [C|ECs], Us);
scan_based_exponent([$_,Next|Cs], St, Line, Col, Toks, B, Ncs, BCs, [Prev|_]=ECs, _) when
      ?DIGIT(Next) andalso ?DIGIT(Prev) ->
    scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, [Next,$_|ECs], with_underscore);
scan_based_exponent([$_]=Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,ECs,Us},fun scan_based_exponent/6}};
scan_based_exponent([]=Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{B,Ncs,BCs,ECs, Us},fun scan_based_exponent/6}};
scan_based_exponent(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us) ->
    based_float_end(Cs, St, Line, Col, Toks, B, Ncs, BCs, ECs, Us).

% Note: the base and exponent parts are always in decimal
based_float_end(Cs, St, Line, Col, Toks, 10, Ncs0, BCs, ECs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    ECs = lists:reverse(ECs0),
    Tcs = BCs ++ Ncs ++ ECs,
    Fcs = case ECs of
              [] -> Ncs ++ ECs;
              [$#|ECs1] -> Ncs ++ ECs1
          end,
    try list_to_float(remove_digit_separators(Fcs, Us)) of
        F ->
            tok3(Cs, St, Line, Col, Toks, float, Tcs, F)
    catch
        _:_ ->
            Ncol = incr_column(Col, length(Ncs) + length(BCs)),
            scan_error({illegal,float}, Line, Col, Line, Ncol, Cs)
    end;
based_float_end(Cs, St, Line, Col, Toks, B, Ncs0, BCs, ECs0, Us) when B =/= 10 ->
    %% there is no general list_to_float(String,Base) available yet
    Ncs = lists:reverse(Ncs0),
    ECs = lists:reverse(ECs0),
    Tcs = BCs ++ Ncs ++ ECs,
    Exp = case ECs of
              [] -> 0;
              [$#,_|ECs1] -> list_to_integer(remove_digit_separators(ECs1, Us))
          end,
    Ncs1 = trim_float_zeros(lists:reverse(trim_float_zeros(remove_digit_separators(Ncs0, Us)))),
    N = list_to_integer(lists:delete($.,Ncs1), B),
    D = length(Ncs1) - string:chr(Ncs1, $.),
    F = N * math:pow(B, Exp-D),
    tok3(Cs, St, Line, Col, Toks, float, Tcs, F).

trim_float_zeros([$0, $. | _]=Cs) -> Cs;  % don't remove 0 next to `.`
trim_float_zeros([$0 | Cs]) -> trim_float_zeros(Cs);
trim_float_zeros(Cs) -> Cs.

scan_fraction(Cs, #erl_scan{}=St, Line, Col, Toks, {Ncs,Us}) ->
    scan_fraction(Cs, St, Line, Col, Toks, Ncs, Us).

scan_fraction([C|Cs], St, Line, Col, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C|Ncs], Us);
scan_fraction([$_,Next|Cs], St, Line, Col, Toks, [Prev|_]=Ncs, _Us) when
      ?DIGIT(Next) andalso ?DIGIT(Prev) ->
    scan_fraction(Cs, St, Line, Col, Toks, [Next,$_|Ncs], with_underscore);
scan_fraction([$_]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_fraction/6}};
scan_fraction([E|Cs], St, Line, Col, Toks, Ncs, Us) when E =:= $e; E =:= $E ->
    scan_exponent_sign(Cs, St, Line, Col, Toks, [E|Ncs], Us);
scan_fraction([C|_]=Cs0, _St, Line, Col, _Toks, Ncs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,float}, Line, Col, Line, incr_column(Col, length(Ncs)), Cs0);
scan_fraction([]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_fraction/6}};
scan_fraction(Cs, St, Line, Col, Toks, Ncs, Us) ->
    float_end(Cs, St, Line, Col, Toks, Ncs, Us).

scan_exponent_sign(Cs, #erl_scan{}=St, Line, Col, Toks, {Ncs, Us}) ->
    scan_exponent_sign(Cs, St, Line, Col, Toks, Ncs, Us).

scan_exponent_sign([C|Cs], St, Line, Col, Toks, Ncs, Us) when
      C =:= $+; C =:= $- ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs], Us);
scan_exponent_sign([]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_exponent_sign/6}};
scan_exponent_sign(Cs, St, Line, Col, Toks, Ncs, Us) ->
    scan_exponent(Cs, St, Line, Col, Toks, Ncs, Us).

scan_exponent(Cs, #erl_scan{}=St, Line, Col, Toks, {Ncs, Us}) ->
    scan_exponent(Cs, St, Line, Col, Toks, Ncs, Us).

scan_exponent([C|Cs], St, Line, Col, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs], Us);
scan_exponent([$_,Next|Cs], St, Line, Col, Toks, [Prev|_]=Ncs, _) when
      ?DIGIT(Next) andalso ?DIGIT(Prev) ->
    scan_exponent(Cs, St, Line, Col, Toks, [Next,$_|Ncs], with_underscore);
scan_exponent([$_]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_exponent/6}};
scan_exponent([C|_]=Cs0, _St, Line, Col, _Toks, Ncs, _Us) when ?NAMECHAR(C) ->
    scan_error({illegal,float}, Line, Col, Line, incr_column(Col, length(Ncs)), Cs0);
scan_exponent([]=Cs, St, Line, Col, Toks, Ncs, Us) ->
    {more,{Cs,St,Col,Toks,Line,{Ncs,Us},fun scan_exponent/6}};
scan_exponent(Cs, St, Line, Col, Toks, Ncs, Us) ->
    float_end(Cs, St, Line, Col, Toks, Ncs, Us).

float_end(Cs, St, Line, Col, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    try list_to_float(remove_digit_separators(Ncs, Us)) of
        F ->
            tok3(Cs, St, Line, Col, Toks, float, Ncs, F)
    catch
        _:_ ->
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,float}, Line, Col, Line, Ncol, Cs)
    end.

skip_comment_fun(Cs, #erl_scan{}=St, Line, Col, Toks, N) ->
    skip_comment(Cs, St, Line, Col, Toks, N).

skip_comment([C|Cs], St, Line, Col, Toks, N) when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            skip_comment(Cs, St, Line, Col, Toks, N+1);
        false ->
            Ncol = incr_column(Col, N+1),
            scan_error({illegal,character}, Line, Col, Line, Ncol, Cs)
    end;
skip_comment([]=Cs, St, Line, Col, Toks, N) ->
    {more,{Cs,St,Col,Toks,Line,N,fun skip_comment_fun/6}};
skip_comment(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

scan_comment_fun(Cs, #erl_scan{}=St, Line, Col, Toks, Ncs) ->
    scan_comment(Cs, St, Line, Col, Toks, Ncs).

scan_comment([C|Cs], St, Line, Col, Toks, Ncs)
  when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            scan_comment(Cs, St, Line, Col, Toks, [C|Ncs]);
        false ->
            Ncol = incr_column(Col, length(Ncs)+1),
            scan_error({illegal,character}, Line, Col, Line, Ncol, Cs)
    end;
scan_comment([]=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_comment_fun/6}};
scan_comment(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    tok3(Cs, St, Line, Col, Toks, comment, Ncs, Ncs).

scan_check("%%ssa%" ++ Cs, St, Line, Col, Toks, _Ncs) ->
    scan_check1(Cs, St, Line, Toks, Col, 7);
scan_check("%%ssa"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%%ss"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%%s"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%%"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%ssa%" ++ Cs, St, Line, Col, Toks, _Ncs) ->
    scan_check1(Cs, St, Line, Toks, Col, 6);
scan_check("%ssa"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%ss"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("%s"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("ssa%" ++ Cs, St, Line, Col, Toks, _Ncs) ->
    scan_check1(Cs, St, Line, Toks, Col, 5);
scan_check("ssa"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("ss"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check("s"=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check([]=Cs, St, Line, Col, Toks, Ncs) ->
    {more,{Cs,St,Col,Toks,Line,Ncs,fun scan_check/6}};
scan_check(Cs, St=#erl_scan{comment=true}, Line, Col, Toks, Ncs) ->
    scan_comment(Cs, St, Line, Col, Toks, Ncs);
scan_check(Cs, St, Line, Col, Toks, _Ncs) ->
    skip_comment(Cs, St, Line, Col, Toks, 1).

scan_check1(Cs, St=#erl_scan{in_check=true}, Line, Toks, Col, NoofCols) ->
    %% Skip as we are already in the check mode
    scan1(Cs, St, Line, incr_column(Col, NoofCols), Toks);
scan_check1(Cs, St, Line, Toks, Col, NoofCols) ->
    tok2(Cs, St#erl_scan{in_check=true}, Line,
         Col, Toks, "%ssa%", '%ssa%', NoofCols).

tok2(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col, Toks, _Wcs, P) ->
    scan1(Cs, St, Line, Col, [{P,anno(Line)}|Toks]);
tok2(Cs, #erl_scan{}=St, Line, Col, Toks, Wcs, P) ->
    Anno = anno(Line, Col, St, ?STR(P, St, Wcs)),
    scan1(Cs, St, Line, incr_column(Col, length(Wcs)), [{P,Anno}|Toks]).

tok2(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col, Toks, _Wcs, P, _N) ->
    scan1(Cs, St, Line, Col, [{P,anno(Line)}|Toks]);
tok2(Cs, #erl_scan{}=St, Line, Col, Toks, Wcs, P, N) ->
    Anno = anno(Line, Col, St, ?STR(P,St,Wcs)),
    scan1(Cs, St, Line, incr_column(Col, N), [{P,Anno}|Toks]).

tok3(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col, Toks, Item, _S, Sym) ->
    scan1(Cs, St, Line, Col, [{Item,anno(Line),Sym}|Toks]);
tok3(Cs, #erl_scan{}=St, Line, Col, Toks, Item, String, Sym) ->
    Token = {Item,anno(Line, Col, St, ?STR(Item, St, String)),Sym},
    scan1(Cs, St, Line, incr_column(Col, length(String)), [Token|Toks]).

tok3(Cs, #erl_scan{has_fun = false}=St, Line, no_col=Col, Toks, Item,
     _String, Sym, _Length) ->
    scan1(Cs, St, Line, Col, [{Item,anno(Line),Sym}|Toks]);
tok3(Cs, #erl_scan{}=St, Line, Col, Toks, Item, String, Sym, Length) ->
    Token = {Item,anno(Line, Col, St, ?STR(Item, St, String)),Sym},
    scan1(Cs, St, Line, incr_column(Col, Length), [Token|Toks]).

scan_error(Error, Line, Col, EndLine, EndCol, Rest) ->
    Loc = location(Line, Col),
    EndLoc = location(EndLine, EndCol),
    scan_error(Error, Loc, EndLoc, Rest).

scan_error(Error, ErrorLoc, EndLoc, Rest) ->
    {{error,{ErrorLoc,?MODULE,Error},EndLoc},Rest}.

-compile({inline,[anno/4]}).

anno(Line, no_col, #erl_scan{has_fun = false}, _String) ->
    anno(Line);
anno(Line, no_col, #erl_scan{has_fun = true}, []) ->
    anno(Line);
anno(Line, no_col, #erl_scan{has_fun = true}, String) ->
    Anno = anno(Line),
    erl_anno:set_text(String, Anno);
anno(Line, Col, #erl_scan{has_fun = false}, _String) ->
    anno({Line, Col});
anno(Line, Col, #erl_scan{has_fun = true}, []) ->
    anno({Line, Col});
anno(Line, Col, #erl_scan{has_fun = true}, String) ->
    Anno = anno({Line, Col}),
    erl_anno:set_text(String, Anno).

location(Line, no_col) ->
    Line;
location(Line, Col) when is_integer(Col) ->
    {Line,Col}.

-compile({inline,[anno/1,incr_column/2,new_column/2,int_column/1]}).

anno(Location) ->
    erl_anno:new(Location).

incr_column(Col, N) when is_integer(N) ->
    if
        Col =:= no_col  -> Col;
        is_integer(Col) -> Col + N
    end.

new_column(no_col, no_col) -> no_col;
new_column(Col, Ncol) when is_integer(Ncol) ->
    if
        Col =:= no_col  -> Col;
        is_integer(Col) -> Ncol
    end.

%% Ensure an integer column for calculations
int_column(no_col) ->
    1;
int_column(Col) when is_integer(Col) ->
    Col.


%% lists:duplicate/3 (not exported)
lists_duplicate(0, _, L) -> L;
lists_duplicate(N, X, L) -> lists_duplicate(N-1, X, [X|L]).

%% lists:foldl/3 over lists:reverse/2
lists_foldl_reverse(Lists, Acc) ->
    lists:foldl(fun lists:reverse/2, Acc, Lists).

nl_spcs(2)  -> "\n ";
nl_spcs(3)  -> "\n  ";
nl_spcs(4)  -> "\n   ";
nl_spcs(5)  -> "\n    ";
nl_spcs(6)  -> "\n     ";
nl_spcs(7)  -> "\n      ";
nl_spcs(8)  -> "\n       ";
nl_spcs(9)  -> "\n        ";
nl_spcs(10) -> "\n         ";
nl_spcs(11) -> "\n          ";
nl_spcs(12) -> "\n           ";
nl_spcs(13) -> "\n            ";
nl_spcs(14) -> "\n             ";
nl_spcs(15) -> "\n              ";
nl_spcs(16) -> "\n               ";
nl_spcs(17) -> "\n                ".

spcs(1)  -> " ";
spcs(2)  -> "  ";
spcs(3)  -> "   ";
spcs(4)  -> "    ";
spcs(5)  -> "     ";
spcs(6)  -> "      ";
spcs(7)  -> "       ";
spcs(8)  -> "        ";
spcs(9)  -> "         ";
spcs(10) -> "          ";
spcs(11) -> "           ";
spcs(12) -> "            ";
spcs(13) -> "             ";
spcs(14) -> "              ";
spcs(15) -> "               ";
spcs(16) -> "                ".

nl_tabs(2)  -> "\n\t";
nl_tabs(3)  -> "\n\t\t";
nl_tabs(4)  -> "\n\t\t\t";
nl_tabs(5)  -> "\n\t\t\t\t";
nl_tabs(6)  -> "\n\t\t\t\t\t";
nl_tabs(7)  -> "\n\t\t\t\t\t\t";
nl_tabs(8)  -> "\n\t\t\t\t\t\t\t";
nl_tabs(9)  -> "\n\t\t\t\t\t\t\t\t";
nl_tabs(10) -> "\n\t\t\t\t\t\t\t\t\t";
nl_tabs(11) -> "\n\t\t\t\t\t\t\t\t\t\t".

tabs(1)  ->  "\t";
tabs(2)  ->  "\t\t";
tabs(3)  ->  "\t\t\t";
tabs(4)  ->  "\t\t\t\t";
tabs(5)  ->  "\t\t\t\t\t";
tabs(6)  ->  "\t\t\t\t\t\t";
tabs(7)  ->  "\t\t\t\t\t\t\t";
tabs(8)  ->  "\t\t\t\t\t\t\t\t";
tabs(9)  ->  "\t\t\t\t\t\t\t\t\t";
tabs(10) ->  "\t\t\t\t\t\t\t\t\t\t".

%% Dynamic version of reserved_word that knows about the possibility
%% that enabled features might change the set of reserved words.
-doc "Returns `true` if `Atom` is an Erlang reserved word, otherwise `false`.".
-spec reserved_word(Atom :: atom()) -> boolean().
reserved_word(Atom) ->
    case f_reserved_word(Atom) of
        true -> true;
        false ->
            lists:member(Atom, erl_features:keywords())
    end.

%% Static version of reserved_words.  These represent the fixed set of
%% reserved words.
-doc false.
f_reserved_word('after') -> true;
f_reserved_word('begin') -> true;
f_reserved_word('case') -> true;
f_reserved_word('try') -> true;
f_reserved_word('cond') -> true;
f_reserved_word('catch') -> true;
f_reserved_word('andalso') -> true;
f_reserved_word('orelse') -> true;
f_reserved_word('end') -> true;
f_reserved_word('fun') -> true;
f_reserved_word('if') -> true;
f_reserved_word('let') -> true;
f_reserved_word('of') -> true;
f_reserved_word('receive') -> true;
f_reserved_word('when') -> true;
f_reserved_word('bnot') -> true;
f_reserved_word('not') -> true;
f_reserved_word('div') -> true;
f_reserved_word('rem') -> true;
f_reserved_word('band') -> true;
f_reserved_word('and') -> true;
f_reserved_word('bor') -> true;
f_reserved_word('bxor') -> true;
f_reserved_word('bsl') -> true;
f_reserved_word('bsr') -> true;
f_reserved_word('or') -> true;
f_reserved_word('xor') -> true;
f_reserved_word(_) -> false.
