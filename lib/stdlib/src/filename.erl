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
-module(filename).
-moduledoc """
Filename manipulation functions.

This module provides functions for analyzing and manipulating filenames. These
functions are designed so that the Erlang code can work on many different
platforms with different filename formats. With filename is meant all strings
that can be used to denote a file. The filename can be a short relative name
like `foo.erl`, a long absolute name including a drive designator, a directory
name like `D:\usr/local\bin\erl/lib\tools\foo.erl`, or any variations in
between.

In Windows, all functions return filenames with forward slashes only, even if
the arguments contain backslashes. To normalize a filename by removing redundant
directory separators, use `join/1`.

The module supports [raw filenames](unicode_usage.md#notes-about-raw-filenames)
in the way that if a binary is present, or the filename cannot be interpreted
according to the return value of `file:native_name_encoding/0`, a raw filename
is also returned. For example, [`join/1`](`join/1`) provided with a path
component that is a binary (and cannot be interpreted under the current native
filename encoding) results in a raw filename that is returned (the join
operation is performed of course). For more information about raw filenames, see
the `m:file` module.

> #### Note {: .info }
>
> Functionality in this module generally assumes valid input and does not
> necessarily fail on input that does not use a valid encoding, but may instead
> very likely produce invalid output.
>
> File operations used to accept filenames containing null characters (integer
> value zero). This caused the name to be truncated and in some cases arguments
> to primitive operations to be mixed up. Filenames containing null characters
> inside the filename are now _rejected_ and will cause primitive file
> operations to fail.

> #### Warning {: .warning }
>
> Currently null characters at the end of the filename will be accepted by
> primitive file operations. Such filenames are however still documented as
> invalid. The implementation will also change in the future and reject such
> filenames.
""".

-removed([{find_src,'_',"use filelib:find_source/1,3 instead"}]).
-removed([{safe_relative_path,1,"use filelib:safe_relative_path/2 instead"}]).

%% Purpose: Provides generic manipulation of filenames.
%%
%% Generally, these functions accept filenames in the native format
%% for the current operating system (Unix or Windows).
%% Deep characters lists (as returned by io_lib:format()) are accepted;
%% resulting strings will always be flat.
%%
%% Implementation note: We used to only flatten if the list turned out
%% to be deep. Now that atoms are allowed in deep lists, in most cases
%% we flatten the arguments immediately on function entry as that makes
%% it easier to ensure that the code works.

%%
%% *** Requirements on Raw Filename Format ***
%%
%% These requirements are due to the 'filename' module
%% in stdlib. This since it is documented that it
%% should be able to operate on raw filenames as well
%% as ordinary filenames.
%%
%% A raw filename *must* be a byte sequence where:
%% 1. Codepoints 0-127 (7-bit ascii) *must* be encoded
%%    as a byte with the corresponding value. That is,
%%    the most significant bit in the byte encoding the
%%    codepoint is never set.
%% 2. Codepoints greater than 127 *must* be encoded
%%    with the most significant bit set in *every* byte
%%    encoding it.
%%
%% Latin1 and UTF-8 meet these requirements while
%% UTF-16 and UTF-32 don't.
%%
%% On Windows filenames are natively stored as malformed
%% UTF-16LE (lonely surrogates may appear). A more correct
%% description than UTF-16 would be an array of 16-bit
%% words... In order to meet the requirements of the
%% raw file format we convert the malformed UTF-16LE to
%% malformed UTF-8 which meet the requirements.
%%
%% Note that these requirements are today only OTP
%% internal (erts-stdlib internal) requirements that
%% could be changed.
%%

-export([absname/1, absname/2, absname_join/2, 
	 basename/1, basename/2, dirname/1,
	 extension/1, join/1, join/2, pathtype/1,
         rootname/1, rootname/2, split/1, flatten/1, nativename/1]).
-export([basedir/2, basedir/3]).
-export([validate/1]).

%% Undocumented and unsupported exports.
-export([append/2]).

-include_lib("kernel/include/file.hrl").

-define(IS_DRIVELETTER(Letter),
        (is_integer(Letter)
         andalso (($A =< Letter andalso Letter =< $Z)
                  orelse ($a =< Letter andalso Letter =< $z)))).

%% Converts a relative filename to an absolute filename
%% or the filename itself if it already is an absolute filename
%% Note that no attempt is made to create the most beatiful
%% absolute name since this can give incorrect results on 
%% file systems which allows links.
%% Examples:
%% Assume (for UNIX) current directory "/usr/local"
%% Assume (for WIN32) current directory "D:/usr/local"
%%  
%% (for Unix) : absname("foo") -> "/usr/local/foo"
%% (for WIN32): absname("foo") -> "D:/usr/local/foo"
%% (for Unix) : absname("../x") -> "/usr/local/../x"
%% (for WIN32): absname("../x") -> "D:/usr/local/../x"
%% (for Unix) : absname("/") -> "/"
%% (for WIN32): absname("/") -> "D:/"


-doc """
Converts a relative `Filename` and returns an absolute name. No attempt is made
to create the shortest absolute name, as this can give incorrect results on file
systems that allow links.

_Unix examples:_

```erlang
1> pwd().
"/usr/local"
2> filename:absname("foo").
"/usr/local/foo"
3> filename:absname("../x").
"/usr/local/../x"
4> filename:absname("/").
"/"
```

_Windows examples:_

```erlang
1> pwd().
"D:/usr/local"
2> filename:absname("foo").
"D:/usr/local/foo"
3> filename:absname("../x").
"D:/usr/local/../x"
4> filename:absname("/").
"D:/"
```
""".
-spec absname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
absname(Name) ->
    {ok, Cwd} = file:get_cwd(),
    absname(Name, Cwd).

-doc """
Same as `absname/1`, except that the directory to which the filename is to be
made relative is specified in argument `Dir`.
""".
-spec absname(Filename, Dir) -> file:filename_all() when
      Filename :: file:name_all(),
      Dir :: file:name_all().
absname(Name, AbsBase) when is_binary(Name), is_list(AbsBase) ->
    absname(Name,filename_string_to_binary(AbsBase));
absname(Name, AbsBase) when is_list(Name), is_binary(AbsBase) ->
    absname(filename_string_to_binary(Name),AbsBase);

absname(Name, AbsBase) ->
    case pathtype(Name) of
	relative ->
	    absname_join(AbsBase, Name);
	absolute ->
	    %% We must flatten the filename before passing it into join/1,
	    %% or we will get slashes inserted into the wrong places.
	    join([flatten(Name)]);
	volumerelative ->
	    absname_vr(split(Name), split(AbsBase), AbsBase)
    end.

%% Handles volumerelative names (on Windows only).

absname_vr([<<"/">>|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    join([Volume|Rest1]);
absname_vr([<<X, $:>>|Rest1], [<<X,_/binary>>|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(join(Rest1), AbsBase);
absname_vr([<<X, $:>>|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case file:get_cwd([X, $:]) of
	    {ok, Dir}  -> filename_string_to_binary(Dir);
	    {error, _} -> <<X, $:, $/>>
    end,
    absname(join(Name), Dcwd);
absname_vr(["/"|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    join([Volume|Rest1]);
absname_vr([[X, $:]|Rest1], [[X|_]|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(join(Rest1), AbsBase);
absname_vr([[X, $:]|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case file:get_cwd([X, $:]) of
	    {ok, Dir}  -> Dir;
	    {error, _} -> [X, $:, $/]
    end,
    absname(join(Name), Dcwd).

%% Joins a relative filename to an absolute base. 
%% This is just a join/2, but assumes that 
%% AbsBase must be absolute and Name must be relative.

-doc """
Joins an absolute directory with a relative filename.

Similar to `join/2`, but on platforms with tight restrictions on raw filename length
and no support for symbolic links, leading parent directory components in `Filename` are matched
against trailing directory components in `Dir` so they can be removed from the
result - minimizing its length.
""".
-spec absname_join(Dir, Filename) -> file:filename_all() when
      Dir :: file:name_all(),
      Filename :: file:name_all().
absname_join(AbsBase, Name) ->
    join(AbsBase, flatten(Name)).

%% Returns the part of the filename after the last directory separator,
%% or the filename itself if it has no separators.
%%
%% Examples: basename("foo") -> "foo"
%%           basename("/usr/foo") -> "foo"
%%           basename("/usr/foo/") -> "foo"  (trailing slashes ignored)
%%           basename("/") -> []

-doc """
Returns the last component of `Filename`, or `Filename` itself if it does not
contain any directory separators.

_Examples:_

```erlang
5> filename:basename("foo").
"foo"
6> filename:basename("/usr/foo").
"foo"
7> filename:basename("/").
[]
```
""".
-spec basename(Filename) -> file:filename_all() when
      Filename :: file:name_all().

basename(Name) when is_binary(Name) ->
    case os:type() of
	{win32,_} ->
	    win_basenameb(Name);
	_ ->
	    basenameb(Name,[<<"/">>])
    end;
    
basename(Name0) ->
    Name1 = flatten(Name0),
    {DirSep2, DrvSep} = separators(),
    Name = skip_prefix(Name1, DrvSep),
    basename1(Name, Name, DirSep2).

win_basenameb(<<Letter,$:,Rest/binary>>) when ?IS_DRIVELETTER(Letter) ->
    basenameb(Rest,[<<"/">>,<<"\\">>]);
win_basenameb(O) ->
    basenameb(O,[<<"/">>,<<"\\">>]).
basenameb(Bin,Sep) ->
    Parts = [ X || X <- binary:split(Bin,Sep,[global]),
		   X =/= <<>> ],
    if
	Parts =:= [] ->
	    <<>>;
	true ->
	    lists:last(Parts)
    end.
    


basename1([$/], Tail0, _DirSep2) ->
    %% End of filename -- must get rid of trailing directory separator.
    [_|Tail] = lists:reverse(Tail0),
    lists:reverse(Tail);
basename1([$/|Rest], _Tail, DirSep2) ->
    basename1(Rest, Rest, DirSep2);
basename1([DirSep2|Rest], Tail, DirSep2) when is_integer(DirSep2) ->
    basename1([$/|Rest], Tail, DirSep2);
basename1([Char|Rest], Tail, DirSep2) when is_integer(Char) ->
    basename1(Rest, Tail, DirSep2);
basename1([], Tail, _DirSep2) ->
    Tail.

skip_prefix(Name, false) ->
    Name;
skip_prefix([L, DrvSep|Name], DrvSep) when ?IS_DRIVELETTER(L) ->
    Name;
skip_prefix(Name, _) ->
    Name.

%% Returns the last component of the filename, with the given
%% extension stripped.  Use this function if you want
%% to remove an extension that might or might not be there.
%% Use rootname(basename(File)) if you want to remove an extension
%% that you know exists, but you are not sure which one it is.
%%
%% Example: basename("~/src/kalle.erl", ".erl") -> "kalle"
%%	    basename("~/src/kalle.jam", ".erl") -> "kalle.jam"
%%	    basename("~/src/kalle.old.erl", ".erl") -> "kalle.old"
%%
%%	    rootname(basename("xxx.jam")) -> "xxx"
%%	    rootname(basename("xxx.erl")) -> "xxx"

-doc """
Returns the last component of `Filename` with extension `Ext` stripped.

This function is to be used to remove a (possible) specific extension.
To remove an existing extension when you are unsure which one it is, use
[`rootname(basename(Filename))`](`rootname/1`).

_Examples:_

```erlang
8> filename:basename("~/src/kalle.erl", ".erl").
"kalle"
9> filename:basename("~/src/kalle.beam", ".erl").
"kalle.beam"
10> filename:basename("~/src/kalle.old.erl", ".erl").
"kalle.old"
11> filename:rootname(filename:basename("~/src/kalle.erl")).
"kalle"
12> filename:rootname(filename:basename("~/src/kalle.beam")).
"kalle"
```
""".
-spec basename(Filename, Ext) -> file:filename_all() when
      Filename :: file:name_all(),
      Ext :: file:name_all().
basename(Name, Ext) when is_binary(Name), is_list(Ext) ->
    basename(Name,filename_string_to_binary(Ext));
basename(Name, Ext) when is_list(Name), is_binary(Ext) ->
    basename(filename_string_to_binary(Name),Ext);
basename(Name, Ext) when is_binary(Name), is_binary(Ext) ->
    BName = basename(Name),
    LAll = byte_size(Name),
    LN = byte_size(BName),
    LE = byte_size(Ext),
    case LN - LE of
	Neg when Neg < 0 ->
	    BName;
	Pos ->
	    StartLen = LAll - Pos - LE,
	    case Name of
		<<_:StartLen/binary,Part:Pos/binary,Ext/binary>> ->
		    Part;
		_Other ->
		    BName
	    end
    end;

basename(Name0, Ext0) ->
    Name = flatten(Name0),
    Ext = flatten(Ext0),
    {DirSep2,DrvSep} = separators(),
    NoPrefix = skip_prefix(Name, DrvSep),
    basename(NoPrefix, Ext, [], DirSep2).

basename(Ext, Ext, Tail, _DrvSep2) ->
    lists:reverse(Tail);
basename([$/|[]], Ext, Tail, DrvSep2) ->
    basename([], Ext, Tail, DrvSep2);
basename([$/|Rest], Ext, _Tail, DrvSep2) ->
    basename(Rest, Ext, [], DrvSep2);
basename([DirSep2|Rest], Ext, Tail, DirSep2) when is_integer(DirSep2) ->
    basename([$/|Rest], Ext, Tail, DirSep2);
basename([Char|Rest], Ext, Tail, DrvSep2) when is_integer(Char) ->
    basename(Rest, Ext, [Char|Tail], DrvSep2);
basename([], _Ext, Tail, _DrvSep2) ->
    lists:reverse(Tail).

%% Returns the directory part of a pathname.
%%
%% Example: dirname("/usr/src/kalle.erl") -> "/usr/src",
%%	    dirname("kalle.erl") -> "."

-doc """
Returns the directory part of `Filename`.

_Examples:_

```erlang
13> filename:dirname("/usr/src/kalle.erl").
"/usr/src"
14> filename:dirname("kalle.erl").
"."
```

```erlang
5> filename:dirname("\\usr\\src/kalle.erl"). % Windows
"/usr/src"
```
""".
-spec dirname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
dirname(Name) when is_binary(Name) ->
    {Dsep,Drivesep} = separators(),
    SList = case Dsep of
		Sep when is_integer(Sep) -> 
		    [ <<Sep>> ];
		_ ->
		    []
	    end,
    {XPart0,Dirs} = case Drivesep of
		       X when is_integer(X) ->
			   case Name of
			       <<DL,X,Rest/binary>> when ?IS_DRIVELETTER(DL) ->
				   {<<DL,X>>,Rest};
			       _ ->
				   {<<>>,Name}
			   end;
		       _ ->
			   {<<>>,Name} 
		   end,
    Parts0 = binary:split(Dirs,[<<"/">>|SList],[global]),
    %% Fairly short lists of parts, OK to reverse twice...
    Parts = case Parts0 of
		[] -> [];
		_ -> lists:reverse(fstrip(tl(lists:reverse(Parts0))))
	    end,
    XPart = case {Parts,XPart0} of
		{[],<<>>} ->
		    <<".">>;
		_ ->
		    XPart0
	    end,
    dirjoin(Parts,XPart,<<"/">>);

dirname(Name0) ->
    Name = flatten(Name0),
    dirname(Name, [], [], separators()).

dirname([$/|Rest], Dir, File, Seps) ->
    dirname(Rest, File++Dir, [$/], Seps);
dirname([DirSep|Rest], Dir, File, {DirSep,_}=Seps) when is_integer(DirSep) ->
    dirname(Rest, File++Dir, [$/], Seps);
dirname([Dl,DrvSep|Rest], [], [], {_,DrvSep}=Seps)
  when is_integer(DrvSep), ?IS_DRIVELETTER(Dl) ->
    dirname(Rest, [DrvSep,Dl], [], Seps);
dirname([Char|Rest], Dir, File, Seps) when is_integer(Char) ->
    dirname(Rest, Dir, [Char|File], Seps);
dirname([], [], File, _Seps) ->
    case lists:reverse(File) of
	[$/|_] -> [$/];
	_ -> "."
    end;
dirname([], [$/|Rest], File, Seps) ->
    dirname([], Rest, File, Seps);
dirname([], [DrvSep,Dl], File, {_,DrvSep}) ->
    case lists:reverse(File) of
	[$/|_] -> [Dl,DrvSep,$/];
	_ -> [Dl,DrvSep]
    end;
dirname([], Dir, _, _) ->
    lists:reverse(Dir).

%% Compatibility with lists variant, remove trailing slashes
fstrip([<<>>,X|Y]) ->
    fstrip([X|Y]);
fstrip(A) ->
    A.
			   

dirjoin([<<>>|T],Acc,Sep) ->
    dirjoin1(T,<<Acc/binary,"/">>,Sep);
dirjoin(A,B,C) ->
    dirjoin1(A,B,C).

dirjoin1([],Acc,_) ->
    Acc;
dirjoin1([One],Acc,_) ->
    <<Acc/binary,One/binary>>;
dirjoin1([H|T],Acc,Sep) ->
    dirjoin(T,<<Acc/binary,H/binary,Sep/binary>>,Sep).

    
%% Given a filename string, returns the file extension,
%% including the period.  Returns an empty list if there
%% is no extension.
%%
%% Example: extension("foo.erl") -> ".erl"
%%	    extension("jam.src/kalle") -> ""
%%
%% On Windows:  fn:dirname("\\usr\\src/kalle.erl") -> "/usr/src"

-doc """
Returns the file extension of `Filename`, including the period. Returns an empty
string if no extension exists.

_Examples:_

```erlang
15> filename:extension("foo.erl").
".erl"
16> filename:extension("beam.src/kalle").
[]
```
""".
-spec extension(Filename) -> file:filename_all() when
      Filename :: file:name_all().
extension(Name) when is_binary(Name) ->
    {Dsep,_} = separators(),
    SList = case Dsep of
		Sep when is_integer(Sep) -> 
		    [ <<Sep>> ];
		_ ->
		    []
	    end,
    case binary:matches(Name,[<<".">>]) of
	[] ->
	    <<>>;
	List ->
	    case lists:last(List) of
		{0,_} ->
		    <<>>;
		{Pos, _} ->
		    <<_:(Pos-1)/binary,Part/binary>> = Name,
		    case binary:match(Part,[<<"/">>|SList]) of
			nomatch ->
			    <<_:Pos/binary,Result/binary>> = Name,
			    Result;
			_ ->
			    <<>>
		    end
	    end
    end;

extension(Name0) ->
    Name = flatten(Name0),
    extension([$/ | Name], [], major_os_type()).

extension([$.|Rest]=Result, _Result, OsType) ->
    extension(Rest, Result, OsType);
extension([$/,$.|Rest], _Result, OsType) ->
    extension(Rest, [], OsType);
extension([$\\,$.|Rest], _Result, win32) ->
    extension(Rest, [], win32);
extension([Char|Rest], [], OsType) when is_integer(Char) ->
    extension(Rest, [], OsType);
extension([$/|Rest], _Result, OsType) ->
    extension(Rest, [], OsType);
extension([$\\|Rest], _Result, win32) ->
    extension(Rest, [], win32);
extension([Char|Rest], Result, OsType) when is_integer(Char) ->
    extension(Rest, Result, OsType);
extension([], Result, _OsType) ->
    Result.

%% Joins a list of filenames with directory separators.

-doc """
Joins a list of filename `Components` with directory separators. If one of the
elements of `Components` includes an absolute path, such as `"/xxx"`, the
preceding elements, if any, are removed from the result.

The result is "normalized":

- Redundant directory separators are removed.
- In Windows, all directory separators are forward slashes and the drive letter
  is in lower case.

_Examples:_

```erlang
17> filename:join(["/usr", "local", "bin"]).
"/usr/local/bin"
18> filename:join(["a/b///c/"]).
"a/b/c"
```

```erlang
6> filename:join(["B:a\\b///c/"]). % Windows
"b:a/b/c"
```
""".
-spec join(Components) -> file:filename_all() when
      Components :: [file:name_all()].
join([Name1, Name2|Rest]) ->
    join([join(Name1, Name2)|Rest]);
join([Name]) when is_list(Name) ->
    join1(Name, [], [], major_os_type());
join([Name]) when is_binary(Name) ->
    join1b(Name, <<>>, [], major_os_type());
join([Name]) when is_atom(Name) ->
    join([atom_to_list(Name)]).

%% Joins two filenames with directory separators.

-doc """
Joins two filename components with directory separators. Equivalent to
[`join([Name1, Name2])`](`join/1`).
""".
-spec join(Name1, Name2) -> file:filename_all() when
      Name1 :: file:name_all(),
      Name2 :: file:name_all().
join(Name1, Name2) when is_list(Name1), is_list(Name2) ->
    OsType = major_os_type(),
    case pathtype(Name2) of
	relative -> join1(Name1, Name2, [], OsType);
	_Other -> join1(Name2, [], [], OsType)
    end;
join(Name1, Name2) when is_binary(Name1), is_list(Name2) ->
    join(Name1,filename_string_to_binary(Name2));
join(Name1, Name2) when is_list(Name1), is_binary(Name2) ->
    join(filename_string_to_binary(Name1),Name2);
join(Name1, Name2) when is_binary(Name1), is_binary(Name2) ->
    OsType = major_os_type(),
    case pathtype(Name2) of
	relative -> join1b(Name1, Name2, [], OsType);
	_Other -> join1b(Name2, <<>>, [], OsType)
    end;
    
join(Name1, Name2) when is_atom(Name1) ->
    join(atom_to_list(Name1), Name2);
join(Name1, Name2) when is_atom(Name2) ->
    join(Name1, atom_to_list(Name2)).

%% Internal function to join an absolute name and a relative name.
%% It is the responsibility of the caller to ensure that RelativeName
%% is relative.

join1([UcLetter, $:|Rest], RelativeName, [], win32)
when is_integer(UcLetter), UcLetter >= $A, UcLetter =< $Z ->
    join1(Rest, RelativeName, [$:, UcLetter+$a-$A], win32);
join1([$\\,$\\|Rest], RelativeName, [], win32) ->
    join1([$/,$/|Rest], RelativeName, [], win32);
join1([$/,$/|Rest], RelativeName, [], win32) ->
    join1(Rest, RelativeName, [$/,$/], win32);
join1([$\\|Rest], RelativeName, Result, win32) ->
    join1([$/|Rest], RelativeName, Result, win32);
join1([$/|Rest], RelativeName, [$., $/|Result], OsType) ->
    join1(Rest, RelativeName, [$/|Result], OsType);
join1([$/|Rest], RelativeName, [$/|Result], OsType) ->
    join1(Rest, RelativeName, [$/|Result], OsType);
join1([], [], Result, OsType) ->
    maybe_remove_dirsep(Result, OsType);
join1([], RelativeName, [$:|Rest], win32) ->
    join1(RelativeName, [], [$:|Rest], win32);
join1([], RelativeName, [$/|Result], OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([], RelativeName, [$., $/|Result], OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([], RelativeName, Result, OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([[_|_]=List|Rest], RelativeName, Result, OsType) ->
    join1(List++Rest, RelativeName, Result, OsType);
join1([[]|Rest], RelativeName, Result, OsType) ->
    join1(Rest, RelativeName, Result, OsType);
join1([Char|Rest], RelativeName, Result, OsType) when is_integer(Char) ->
    join1(Rest, RelativeName, [Char|Result], OsType);
join1([Atom|Rest], RelativeName, Result, OsType) when is_atom(Atom) ->
    join1(atom_to_list(Atom)++Rest, RelativeName, Result, OsType).

join1b(<<UcLetter, $:, Rest/binary>>, RelativeName, [], win32)
when is_integer(UcLetter), UcLetter >= $A, UcLetter =< $Z ->
    join1b(Rest, RelativeName, [$:, UcLetter+$a-$A], win32);
join1b(<<$\\,$\\,Rest/binary>>, RelativeName, [], win32) ->
    join1b(<<$/,$/,Rest/binary>>, RelativeName, [], win32);
join1b(<<$/,$/,Rest/binary>>, RelativeName, [], win32) ->
    join1b(Rest, RelativeName, [$/,$/], win32);
join1b(<<$\\,Rest/binary>>, RelativeName, Result, win32) ->
    join1b(<<$/,Rest/binary>>, RelativeName, Result, win32);
join1b(<<$/,Rest/binary>>, RelativeName, [$., $/|Result], OsType) ->
    join1b(Rest, RelativeName, [$/|Result], OsType);
join1b(<<$/,Rest/binary>>, RelativeName, [$/|Result], OsType) ->
    join1b(Rest, RelativeName, [$/|Result], OsType);
join1b(<<>>, <<>>, Result, OsType) ->
    list_to_binary(maybe_remove_dirsep(Result, OsType));
join1b(<<>>, RelativeName, [$:|Rest], win32) ->
    join1b(RelativeName, <<>>, [$:|Rest], win32);
join1b(<<>>, RelativeName, [$/,$/|Result], win32) ->
    join1b(RelativeName, <<>>, [$/,$/|Result], win32);
join1b(<<>>, RelativeName, [$/|Result], OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<>>, RelativeName, [$., $/|Result], OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<>>, RelativeName, Result, OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<Char,Rest/binary>>, RelativeName, Result, OsType) when is_integer(Char) ->
    join1b(Rest, RelativeName, [Char|Result], OsType).

maybe_remove_dirsep([$/, $:, Letter], win32) ->
    [Letter, $:, $/];
maybe_remove_dirsep([$/], _) ->
    [$/];
maybe_remove_dirsep([$/,$/], win32) ->
    [$/,$/];
maybe_remove_dirsep([$/|Name], _) ->
    lists:reverse(Name);
maybe_remove_dirsep(Name, _) ->
    lists:reverse(Name).

%% Appends a directory separator and a pathname component to
%% a given base directory, which is is assumed to be normalised
%% by a previous call to join/{1,2}.

-doc false.
-spec append(file:filename_all(), file:name_all()) -> file:filename_all().
append(Dir, Name) when is_binary(Dir), is_binary(Name) ->
    <<Dir/binary,$/:8,Name/binary>>;
append(Dir, Name) when is_binary(Dir) ->
    append(Dir,filename_string_to_binary(Name));
append(Dir, Name) when is_binary(Name) ->
    append(filename_string_to_binary(Dir),Name);
append(Dir, Name) ->
    Dir ++ [$/|Name].

%% Returns one of absolute, relative or volumerelative.
%%
%% absolute	The pathname refers to a specific file on a specific
%%		volume.  Example: /usr/local/bin/ (on Unix),
%%		h:/port_test (on Windows).
%% relative	The pathname is relative to the current working directory
%%		on the current volume.  Example:  foo/bar, ../src
%% volumerelative  The pathname is relative to the current working directory
%%		on the specified volume, or is a specific file on the
%%		current working volume.  (Windows only)
%%		Example: a:bar.erl, /temp/foo.erl

-doc """
Returns the path type, which is one of the following:

- **`absolute`** - The path name refers to a specific file on a specific volume.

  Unix example: `/usr/local/bin`

  Windows example: `D:/usr/local/bin`

- **`relative`** - The path name is relative to the current working directory on
  the current volume.

  Example: `foo/bar, ../src`

- **`volumerelative`** - The path name is relative to the current working
  directory on a specified volume, or it is a specific file on the current
  working volume.

  Windows example: `D:bar.erl, /bar/foo.erl`
""".
-spec pathtype(Path) -> 'absolute' | 'relative' | 'volumerelative' when
      Path :: file:name_all().
pathtype(Atom) when is_atom(Atom) ->
    pathtype(atom_to_list(Atom));
pathtype(Name) when is_list(Name) or is_binary(Name) ->
    case os:type() of
	{win32, _} ->
	    win32_pathtype(Name);
	{_, _}  ->
	    unix_pathtype(Name)
    end.

unix_pathtype(<<$/,_/binary>>) ->
    absolute;
unix_pathtype([$/|_]) ->
    absolute;
unix_pathtype([List|Rest]) when is_list(List) ->
    unix_pathtype(List++Rest);
unix_pathtype([Atom|Rest]) when is_atom(Atom) ->
    unix_pathtype(atom_to_list(Atom)++Rest);
unix_pathtype(_) ->
    relative.

win32_pathtype([List|Rest]) when is_list(List) ->
    win32_pathtype(List++Rest);
win32_pathtype([Atom|Rest]) when is_atom(Atom) ->
    win32_pathtype(atom_to_list(Atom)++Rest);
win32_pathtype([Char, List|Rest]) when is_list(List) ->
    win32_pathtype([Char|List++Rest]);
win32_pathtype(<<$/, $/, _/binary>>) -> absolute;
win32_pathtype(<<$\\, $/, _/binary>>) -> absolute;
win32_pathtype(<<$/, $\\, _/binary>>) -> absolute;
win32_pathtype(<<$\\, $\\, _/binary>>) -> absolute;
win32_pathtype(<<$/, _/binary>>) -> volumerelative;
win32_pathtype(<<$\\, _/binary>>) -> volumerelative;
win32_pathtype(<<_Letter, $:, $/, _/binary>>) -> absolute;
win32_pathtype(<<_Letter, $:, $\\, _/binary>>) -> absolute;
win32_pathtype(<<_Letter, $:, _/binary>>) -> volumerelative;
win32_pathtype([$/, $/|_]) -> absolute;
win32_pathtype([$\\, $/|_]) -> absolute;
win32_pathtype([$/, $\\|_]) -> absolute;
win32_pathtype([$\\, $\\|_]) -> absolute;
win32_pathtype([$/|_]) -> volumerelative;
win32_pathtype([$\\|_]) -> volumerelative;
win32_pathtype([C1, C2, List|Rest]) when is_list(List) ->
    pathtype([C1, C2|List++Rest]);
win32_pathtype([_Letter, $:, $/|_]) -> absolute;
win32_pathtype([_Letter, $:, $\\|_]) -> absolute;
win32_pathtype([_Letter, $:|_]) -> volumerelative;
win32_pathtype(_) 		  -> relative.

%% Returns all characters in the filename, except the extension.
%%
%% Examples: rootname("/jam.src/kalle") -> "/jam.src/kalle"
%%           rootname("/jam.src/foo.erl") -> "/jam.src/foo"

-doc """
Removes the filename extension.

_Examples:_

```erlang
1> filename:rootname("/beam.src/kalle").
"/beam.src/kalle"
2> filename:rootname("/beam.src/foo.erl").
"/beam.src/foo"
```
""".
-spec rootname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
rootname(Name) when is_binary(Name) ->
    list_to_binary(rootname(binary_to_list(Name))); % No need to handle unicode, . is < 128
rootname(Name0) ->
    Name = flatten(Name0),
    rootname(Name, [], [], major_os_type()).

rootname([$/|Rest], Root, Ext, OsType) ->
    rootname(Rest, [$/]++Ext++Root, [], OsType);
rootname([$\\|Rest], Root, Ext, win32) ->
    rootname(Rest, [$/]++Ext++Root, [], win32);
rootname([$.|Rest], [$/|_]=Root, [], OsType) ->
    rootname(Rest, [$.|Root], [], OsType);
rootname([$.|Rest], Root, Ext, OsType) ->
    rootname(Rest, Ext++Root, ".", OsType);
rootname([Char|Rest], Root, [], OsType) when is_integer(Char) ->
    rootname(Rest, [Char|Root], [], OsType);
rootname([Char|Rest], Root, Ext, OsType) when is_integer(Char) ->
    rootname(Rest, Root, [Char|Ext], OsType);
rootname([], Root, _Ext, _OsType) ->
    lists:reverse(Root).

%% Returns all characters in the filename, except the given extension.
%% If the filename has another extension, the complete filename is
%% returned.
%%
%% Examples: rootname("/jam.src/kalle.jam", ".erl") -> "/jam.src/kalle.jam"
%%           rootname("/jam.src/foo.erl", ".erl") -> "/jam.src/foo"

-doc """
Removes the filename extension `Ext` from `Filename`.

_Examples:_

```erlang
1> filename:rootname("/beam.src/foo.erl", ".erl").
"/beam.src/foo"
2> filename:rootname("/beam.src/foo.beam", ".erl").
"/beam.src/foo.beam"
```
""".
-spec rootname(Filename, Ext) -> file:filename_all() when
      Filename :: file:name_all(),
      Ext :: file:name_all().
rootname(Name, Ext) when is_binary(Name), is_binary(Ext) ->
    list_to_binary(rootname(binary_to_list(Name),binary_to_list(Ext)));
rootname(Name, Ext) when is_binary(Name) ->
    rootname(Name,filename_string_to_binary(Ext));
rootname(Name, Ext) when is_binary(Ext) ->
    rootname(filename_string_to_binary(Name),Ext);
rootname(Name0, Ext0) ->
    Name = flatten(Name0),
    Ext = flatten(Ext0),
    rootname2(Name, Ext, [], major_os_type()).

rootname2(Ext, Ext, [$/|_]=Result, _OsType) ->
    lists:reverse(Result, Ext);
rootname2(Ext, Ext, [$\\|_]=Result, win32) ->
    lists:reverse(Result, Ext);
rootname2(Ext, Ext, Result, _OsType) ->
    lists:reverse(Result);
rootname2([], _Ext, Result, _OsType) ->
    lists:reverse(Result);
rootname2([Char|Rest], Ext, Result, OsType) when is_integer(Char) ->
    rootname2(Rest, Ext, [Char|Result], OsType).

%% Returns a list whose elements are the path components in the filename.
%%
%% Examples:	
%% split("/usr/local/bin") -> ["/", "usr", "local", "bin"]
%% split("foo/bar") -> ["foo", "bar"]
%% split("a:\\msdev\\include") -> ["a:/", "msdev", "include"]

-doc """
Returns a list whose elements are the path components of `Filename`.

_Examples:_

```erlang
24> filename:split("/usr/local/bin").
["/","usr","local","bin"]
25> filename:split("foo/bar").
["foo","bar"]
26> filename:split("a:\\msdev\\include").
["a:/","msdev","include"]
```
""".
-spec split(Filename) -> Components when
      Filename :: file:name_all(),
      Components :: [file:name_all()].
split(Name) when is_binary(Name) ->
    case os:type() of
	{win32, _} -> win32_splitb(Name);
	_  -> unix_splitb(Name)
    end;

split(Name0) ->
    Name = flatten(Name0),
    case os:type() of
	{win32, _} -> win32_split(Name);
	_  -> unix_split(Name)
    end.


unix_splitb(Name) ->
    L = binary:split(Name,[<<"/">>],[global]),
    LL = case L of
	     [<<>>|Rest] when Rest =/= [] ->
		 [<<"/">>|Rest];
	     _ ->
		 L
	 end,
    [ X || X <- LL, X =/= <<>>].


fix_driveletter(Letter0) ->
    if
	Letter0 >= $A, Letter0 =< $Z ->  
	    Letter0+$a-$A;
	true ->
	    Letter0
    end.
win32_splitb(<<Letter0,$:, Slash, Rest/binary>>) when (((Slash =:= $\\) orelse (Slash =:= $/)) andalso
							 ?IS_DRIVELETTER(Letter0)) ->
    Letter = fix_driveletter(Letter0),
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<Letter,$:,$/>> | [ X || X <- L, X =/= <<>> ]]; 
win32_splitb(<<Letter0,$:,Rest/binary>>) when ?IS_DRIVELETTER(Letter0) ->
    Letter = fix_driveletter(Letter0),
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<Letter,$:>> | [ X || X <- L, X =/= <<>> ]];
win32_splitb(<<Slash,Slash,Rest/binary>>) when ((Slash =:= $\\) orelse (Slash =:= $/)) ->
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<"//">> | [ X || X <- L, X =/= <<>> ]];
win32_splitb(<<Slash,Rest/binary>>) when ((Slash =:= $\\) orelse (Slash =:= $/)) ->
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<$/>> | [ X || X <- L, X =/= <<>> ]];
win32_splitb(Name) ->
    L = binary:split(Name,[<<"/">>,<<"\\">>],[global]),
    [ X || X <- L, X =/= <<>> ].
    

unix_split(Name) ->
    split(Name, [], unix).

win32_split([Slash,Slash|Rest]) when ((Slash =:= $\\) orelse (Slash =:= $/)) ->
    split(Rest, [[$/,$/]], win32);
win32_split([$\\|Rest]) ->
    win32_split([$/|Rest]);
win32_split([X, $\\|Rest]) when is_integer(X) ->
    win32_split([X, $/|Rest]);
win32_split([X, Y, $\\|Rest]) when is_integer(X), is_integer(Y) ->
    win32_split([X, Y, $/|Rest]);
win32_split([UcLetter, $:|Rest])
  when is_integer(UcLetter), $A =< UcLetter, UcLetter =< $Z ->
    win32_split([UcLetter+$a-$A, $:|Rest]);
win32_split([Letter, $:, $/|Rest]) ->
    split(Rest, [], [[Letter, $:, $/]], win32);
win32_split([Letter, $:|Rest]) ->
    split(Rest, [], [[Letter, $:]], win32);
win32_split(Name) ->
    split(Name, [], win32).

split([$/|Rest], Components, OsType) ->
    split(Rest, [], [[$/]|Components], OsType);
split([$\\|Rest], Components, win32) ->
    split(Rest, [], [[$/]|Components], win32);
split(RelativeName, Components, OsType) ->
    split(RelativeName, [], Components, OsType).

split([$\\|Rest], Comp, Components, win32) ->
    split([$/|Rest], Comp, Components, win32);
split([$/|Rest], [], Components, OsType) ->
    split(Rest, [], Components, OsType);
split([$/|Rest], Comp, Components, OsType) ->
    split(Rest, [], [lists:reverse(Comp)|Components], OsType);
split([Char|Rest], Comp, Components, OsType) when is_integer(Char) ->
    split(Rest, [Char|Comp], Components, OsType);
split([], [], Components, _OsType) ->
    lists:reverse(Components);
split([], Comp, Components, OsType) ->
    split([], [], [lists:reverse(Comp)|Components], OsType).

%% Converts a filename to a form accepedt by the command shell and native
%% applications on the current platform.  On Windows, forward slashes
%% will be converted to backslashes.  On all platforms, the
%% name will be normalized as done by join/1.

-doc """
Converts `Path` to a form accepted by the command shell and native applications
on the current platform. On Windows, forward slashes are converted to backward
slashes. On all platforms, the name is normalized as done by `join/1`.

_Examples:_

```erlang
19> filename:nativename("/usr/local/bin/"). % Unix
"/usr/local/bin"
```

```erlang
7> filename:nativename("/usr/local/bin/"). % Windows
"\\usr\\local\\bin"
```
""".
-spec nativename(Path) -> file:filename_all() when
      Path :: file:name_all().
nativename(Name0) ->
    Name = join([Name0]),			%Normalize.
    case os:type() of
	{win32, _} -> win32_nativename(Name);
	_          -> Name
    end.

win32_nativename(Name) when is_binary(Name) ->
    binary:replace(Name, <<"/">>, <<"\\">>, [global]);
win32_nativename([$/|Rest]) ->
    [$\\|win32_nativename(Rest)];
win32_nativename([C|Rest]) ->
    [C|win32_nativename(Rest)];
win32_nativename([]) ->
    [].

separators() ->
    case os:type() of
	{win32, _} -> {$\\, $:};
	_ -> {false, false}
    end.

major_os_type() ->
    {OsT, _} = os:type(),
    OsT.

%% flatten(List)
%%  Flatten a list, also accepting atoms.

-doc """
Converts a possibly deep list filename consisting of characters and atoms into
the corresponding flat string filename.
""".
-spec flatten(Filename) -> file:filename_all() when
      Filename :: file:name_all().
flatten(Bin) when is_binary(Bin) ->
    Bin;
flatten(List) ->
    do_flatten(List, []).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) when is_atom(H) ->
    atom_to_list(H) ++ do_flatten(T, Tail);
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail;
do_flatten(Atom, Tail) when is_atom(Atom) ->
    atom_to_list(Atom) ++ flatten(Tail).

filename_string_to_binary(List) ->
    case unicode:characters_to_binary(flatten(List),unicode,file:native_name_encoding()) of
	{error,_,_} ->
	    erlang:error(badarg);
	Bin when is_binary(Bin) ->
	    Bin
    end.

%% Application Base Directories
%% basedir
%% http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

-type basedir_path_type() :: 'user_cache' | 'user_config' | 'user_data'
                           | 'user_log'.
-type basedir_paths_type() :: 'site_config' | 'site_data'.

-type basedir_opts() :: #{author => string() | binary(),
                          os => 'windows' | 'darwin' | 'linux',
                          version => string() | binary()}.

-doc """
Equivalent to [basedir(PathType, Application, #\{\})](`basedir/3`)
or [basedir(PathsType, Application, #\{\})](`basedir/3`).
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec basedir(PathType,Application) -> file:filename_all() when
      PathType :: basedir_path_type(),
      Application :: string() | binary();
             (PathsType,Application) -> [file:filename_all()] when
      PathsType :: basedir_paths_type(),
      Application :: string() | binary().

basedir(Type,Application) when is_atom(Type), is_list(Application) orelse
                                              is_binary(Application) ->
    basedir(Type, Application, #{}).

-doc """
Returns a suitable path, or paths, for a given type.

If `os` is not set in `Opts` the function will default to the native option, that
is `'linux'`, `'darwin'` or `'windows'`, as understood by `os:type/0`.
Anything not recognized as `'darwin'` or `'windows'` is interpreted as `'linux'`.

The options `'author'` and `'version'` are only used with `'windows'` option
mode.

- `user_cache`{: #user_cache }

  The path location is intended for transient data files on a local machine.

  On Linux: Respects the os environment variable `XDG_CACHE_HOME`.

  ```erlang
  1> filename:basedir(user_cache, "my_application", #{os=>linux}).
  "/home/otptest/.cache/my_application"
  ```

  On Darwin:

  ```erlang
  1> filename:basedir(user_cache, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Caches/my_application"
  ```

  On Windows:

  ```erlang
  1> filename:basedir(user_cache, "My App").
  "c:/Users/otptest/AppData/Local/My App/Cache"
  2> filename:basedir(user_cache, "My App").
  "c:/Users/otptest/AppData/Local/My App/Cache"
  3> filename:basedir(user_cache, "My App", #{author=>"Erlang"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/Cache"
  4> filename:basedir(user_cache, "My App", #{version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/My App/1.2/Cache"
  5> filename:basedir(user_cache, "My App", #{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2/Cache"
  ```

- `user_config`{: #user_config }

  The path location is intended for persistent configuration files.

  On Linux: Respects the os environment variable `XDG_CONFIG_HOME`.

  ```erlang
  2> filename:basedir(user_config, "my_application", #{os=>linux}).
  "/home/otptest/.config/my_application"
  ```

  On Darwin:

  ```erlang
  2> filename:basedir(user_config, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Application Support/my_application"
  ```

  On Windows:

  ```erlang
  1> filename:basedir(user_config, "My App").
  "c:/Users/otptest/AppData/Roaming/My App"
  2> filename:basedir(user_config, "My App", #{author=>"Erlang", version=>"1.2"}).
  "c:/Users/otptest/AppData/Roaming/Erlang/My App/1.2"
  ```

- `user_data`{: #user_data }

  The path location is intended for persistent data files.

  On Linux: Respects the os environment variable `XDG_DATA_HOME`.

  ```erlang
  3> filename:basedir(user_data, "my_application", #{os=>linux}).
  "/home/otptest/.local/my_application"
  ```

  On Darwin:

  ```erlang
  3> filename:basedir(user_data, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Application Support/my_application"
  ```

  On Windows:

  ```erlang
  8> filename:basedir(user_data, "My App").
  "c:/Users/otptest/AppData/Local/My App"
  9> filename:basedir(user_data, "My App",#{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2"
  ```

- `user_log`{: #user_log }

  The path location is intended for transient log files on a local machine.

  On Linux: Respects the os environment variable `XDG_CACHE_HOME`.

  ```erlang
  4> filename:basedir(user_log, "my_application", #{os=>linux}).
  "/home/otptest/.cache/my_application/log"
  ```

  On Darwin:

  ```erlang
  4> filename:basedir(user_log, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Logs/my_application"
  ```

  On Windows:

  ```erlang
  12> filename:basedir(user_log, "My App").
  "c:/Users/otptest/AppData/Local/My App/Logs"
  13> filename:basedir(user_log, "My App",#{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2/Logs"
  ```

- `site_config`{: #site_config }

  On Linux: Respects the os environment variable `XDG_CONFIG_DIRS`.

  ```erlang
  5> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/usr/local/share/my_application",
   "/usr/share/my_application"]
  6> os:getenv("XDG_CONFIG_DIRS").
  "/etc/xdg/xdg-ubuntu:/usr/share/upstart/xdg:/etc/xdg"
  7> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/etc/xdg/xdg-ubuntu/my_application",
   "/usr/share/upstart/xdg/my_application",
   "/etc/xdg/my_application"]
  8> os:unsetenv("XDG_CONFIG_DIRS").
  true
  9> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/etc/xdg/my_application"]
  ```

  On Darwin:

  ```erlang
  5> filename:basedir(site_config, "my_application", #{os=>darwin}).
  ["/Library/Application Support/my_application"]
  ```

- `site_data`{: #site_data }

  On Linux: Respects the os environment variable `XDG_DATA_DIRS`.

  ```erlang
  10> os:getenv("XDG_DATA_DIRS").
  "/usr/share/ubuntu:/usr/share/gnome:/usr/local/share/:/usr/share/"
  11> filename:basedir(site_data, "my_application", #{os=>linux}).
  ["/usr/share/ubuntu/my_application",
   "/usr/share/gnome/my_application",
   "/usr/local/share/my_application",
   "/usr/share/my_application"]
  12> os:unsetenv("XDG_DATA_DIRS").
  true
  13> filename:basedir(site_data, "my_application", #{os=>linux}).
  ["/usr/local/share/my_application",
   "/usr/share/my_application"]
  ```

  On Darwin:

  ```erlang
  5> filename:basedir(site_data, "my_application", #{os=>darwin}).
  ["/Library/Application Support/my_application"]
  ```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec basedir(PathType,Application,Opts) -> file:filename_all() when
      PathType :: basedir_path_type(),
      Application :: string() | binary(),
      Opts :: basedir_opts();
             (PathsType,Application,Opts) -> [file:filename_all()] when
      PathsType :: basedir_paths_type(),
      Application :: string() | binary(),
      Opts :: basedir_opts().

basedir(Type,Application,Opts) when is_atom(Type), is_map(Opts),
                                    is_list(Application) orelse
                                    is_binary(Application) ->
    Os   = basedir_os_from_opts(Opts),
    Name = basedir_name_from_opts(Os,Application,Opts),
    Base = basedir_from_os(Type,Os),
    case {Type,Os} of
        {user_log,linux} ->
            filename:join([Base,Name,"log"]);
        {user_log,windows} ->
            filename:join([Base,Name,"Logs"]);
        {user_cache,windows} ->
            filename:join([Base,Name,"Cache"]);
        {Type,_} when Type =:= site_config orelse Type =:= site_data ->
            [filename:join([B,Name]) || B <- Base];
        _ ->
            filename:join([Base,Name])
    end.

basedir_os_from_opts(#{os := linux}) -> linux;
basedir_os_from_opts(#{os := windows}) -> windows;
basedir_os_from_opts(#{os := darwin}) -> darwin;
basedir_os_from_opts(#{}) -> basedir_os_type().

basedir_name_from_opts(windows,App,#{author:=Author,version:=Vsn}) ->
    filename:join([Author,App,Vsn]);
basedir_name_from_opts(windows,App,#{author:=Author}) ->
    filename:join([Author,App]);
basedir_name_from_opts(_,App,#{version:=Vsn}) ->
    filename:join([App,Vsn]);
basedir_name_from_opts(_,App,_) ->
    App.

basedir_from_os(Type,Os) ->
    case Os of
        linux   -> basedir_linux(Type);
        darwin  -> basedir_darwin(Type);
        windows -> basedir_windows(Type)
    end.

-define(basedir_linux_user_data,   ".local/share").
-define(basedir_linux_user_config, ".config").
-define(basedir_linux_user_cache,  ".cache").
-define(basedir_linux_user_log,    ".cache"). %% .cache/App/log
-define(basedir_linux_site_data,   "/usr/local/share/:/usr/share/").
-define(basedir_linux_site_config, "/etc/xdg").

basedir_linux(Type) ->
    case Type of
        user_data   -> getenv("XDG_DATA_HOME",  ?basedir_linux_user_data,  true);
        user_config -> getenv("XDG_CONFIG_HOME",?basedir_linux_user_config,true);
        user_cache  -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_cache, true);
        user_log    -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_log,   true);
        site_data   ->
            Base = getenv("XDG_DATA_DIRS",?basedir_linux_site_data,false),
            string:lexemes(Base, ":");
        site_config ->
            Base = getenv("XDG_CONFIG_DIRS",?basedir_linux_site_config,false),
            string:lexemes(Base, ":")
    end.

-define(basedir_darwin_user_data,   "Library/Application Support").
-define(basedir_darwin_user_config, "Library/Application Support").
-define(basedir_darwin_user_cache,  "Library/Caches").
-define(basedir_darwin_user_log,    "Library/Logs").
-define(basedir_darwin_site_data,   "/Library/Application Support").
-define(basedir_darwin_site_config, "/Library/Application Support").

basedir_darwin(Type) ->
    case Type of
        user_data   -> basedir_join_home(?basedir_darwin_user_data);
        user_config -> basedir_join_home(?basedir_darwin_user_config);
        user_cache  -> basedir_join_home(?basedir_darwin_user_cache);
        user_log    -> basedir_join_home(?basedir_darwin_user_log);
        site_data   -> [?basedir_darwin_site_data];
        site_config -> [?basedir_darwin_site_config]
    end.

%% On Windows:
%% ex. C:\Users\egil\AppData\Local\Ericsson\Erlang
%% %LOCALAPPDATA% is defined on Windows 7 and onwards
%% %APPDATA% is used instead of %LOCALAPPDATA% if it's not defined.
%% %APPDATA% is used for roaming, i.e. for user_config on Windows 7 and beyond.
%%
%% user_data    %LOCALAPPDATA%[/$author]/$appname[/$version]
%% user_config  %APPDATA%[/$author]/$appname[/$version]
%% user_cache   %LOCALAPPDATA%[/$author]/$appname[/$version]/Cache
%% user_log     %LOCALAPPDATA%[/$author]/$appname[/$version]/Logs

-define(basedir_windows_user_data,   "Local").
-define(basedir_windows_user_config, "Roaming").
-define(basedir_windows_user_cache,  "Local").    %% Cache is added later
-define(basedir_windows_user_log,    "Local").    %% Logs is added later

basedir_windows(Type) ->
    %% If LOCALAPPDATA is not defined we are likely on an
    %% XP machine. Use APPDATA instead.
    case basedir_windows_appdata() of
        noappdata ->
            %% No AppData is set
            %% Probably running MSYS
            case Type of
                user_data   -> basedir_join_home(?basedir_windows_user_data);
                user_config -> basedir_join_home(?basedir_windows_user_config);
                user_cache  -> basedir_join_home(?basedir_windows_user_cache);
                user_log    -> basedir_join_home(?basedir_windows_user_log);
                site_data   -> [];
                site_config -> []
            end;
        {ok, AppData} ->
            case Type of
                user_data   -> getenv("LOCALAPPDATA", AppData);
                user_config -> AppData;
                user_cache  -> getenv("LOCALAPPDATA", AppData);
                user_log    -> getenv("LOCALAPPDATA", AppData);
                site_data   -> [];
                site_config -> []
            end
    end.

basedir_windows_appdata() ->
    case os:getenv("APPDATA") of
        Invalid when Invalid =:= false orelse Invalid =:= [] ->
            noappdata;
        Val ->
            {ok, Val}
    end.

%% basedir aux

getenv(K,Def,false) -> getenv(K,Def);
getenv(K,Def,true)  -> getenv(K,basedir_join_home(Def)).

getenv(K,Def) ->
    case os:getenv(K) of
        []    -> Def;
        false -> Def;
        Val   -> Val
    end.

basedir_join_home(Dir) ->
    case os:getenv("HOME") of
        false ->
            {ok,[[Home]]} = init:get_argument(home),
            filename:join(Home,Dir);
        Home  -> filename:join(Home,Dir)
    end.

basedir_os_type() ->
    case os:type() of
        {unix,darwin} -> darwin;
        {win32,_}     -> windows;
        _             -> linux
    end.

%%
%% validate/1
%%

-doc false.
-spec validate(FileName) -> boolean() when
      FileName :: file:name_all().

validate(FileName) when is_binary(FileName) ->
    %% Raw filename...
    validate_bin(FileName);
validate(FileName) when is_list(FileName);
                        is_atom(FileName) ->
    validate_list(FileName,
                  file:native_name_encoding(),
                  os:type()).

validate_list(FileName, Enc, Os) ->
    try
        true = validate_list(FileName, Enc, Os, 0) > 0
    catch
        _ : _ -> false
    end.

validate_list([], _Enc, _Os, Chars) ->
    Chars;
validate_list(C, Enc, Os, Chars) when is_integer(C) ->
    validate_char(C, Enc, Os),
    Chars+1;
validate_list(A, Enc, Os, Chars) when is_atom(A) ->
    validate_list(atom_to_list(A), Enc, Os, Chars);
validate_list([H|T], Enc, Os, Chars) ->
    NewChars = validate_list(H, Enc, Os, Chars),
    validate_list(T, Enc, Os, NewChars).

%% C is always an integer...
% validate_char(C, _, _) when not is_integer(C) ->
%     throw(invalid);
validate_char(C, _, _) when C < 1 ->
    throw(invalid); %% No negative or null characters...
validate_char(C, latin1, _) when C > 255 ->
    throw(invalid);
validate_char(C, utf8, _) when C >= 16#110000 ->
    throw(invalid);
validate_char(C, utf8, {win32, _}) when C > 16#ffff ->
    throw(invalid); %% invalid win wchar...
validate_char(_C, utf8, {win32, _}) ->
    ok; %% Range below is accepted on windows...
validate_char(C, utf8, _) when 16#D800 =< C, C =< 16#DFFF ->
    throw(invalid); %% invalid unicode range...
validate_char(_, _, _) ->
    ok.

validate_bin(Bin) ->
    %% Raw filename. That is, we do not interpret
    %% the encoding, but we still do not accept
    %% null characters...
    try
        true = validate_bin(Bin, 0) > 0
    catch
        _ : _ -> false
    end.

validate_bin(<<>>, Bs) ->
    Bs;
validate_bin(<<0, _Rest/binary>>, _Bs) ->
    throw(invalid); %% No null characters allowed...
validate_bin(<<_B, Rest/binary>>, Bs) ->
    validate_bin(Rest, Bs+1).
