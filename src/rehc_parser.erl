%-------------------------------------------------------------------------------
%
%    rehc: remote erlang host control    
%    Copyright (C) 2013  Jorge Garrido <jorge.garrido@morelosoft.com> [zgbjgg]
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
%-------------------------------------------------------------------------------
-module(rehc_parser).

-vsn("1.0").

-export([get_config/1]).

-include("rehc.hrl").

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create the configuration, read the files and parse them, for each 
%% file, create a new list with the parameters used internally 
%% by the system.
%%
%% @spec get_config(Dir :: string()) -> {ok, Files :: list()}
%% @end
%%--------------------------------------------------------------------
-spec get_config(Dir :: string()) -> {ok, Files :: list()}.
get_config(Dir) ->
    Files = lists:filter(fun(X) -> filelib:is_file(X) end,
			 filelib:wildcard(Dir ++ "/*.rehc")),
    {ok, unzipped_files(Files)}.

%%%===================================================================
%%% PRIVATE API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Unzipped files values, with file:consult/1
%%
%% @spec unzipped_files(Files :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec unzipped_files(Files :: list()) -> list().
unzipped_files([])             -> [];
unzipped_files([File | Files]) ->
    {ok, Unzip} = file:consult(File),
    [ Unzip | unzipped_files(Files)].
    
%% Unzip file (deprecated & actually no used)
%% unzip(IoDev, Acc) ->
%%    case io:get_line(IoDev, "") of
%%	eof    ->
%%	    file:close(IoDev),
%%	    Acc;
%%	Line   ->
%%	    [Flag,Value,_]=re:split(Line, "[\"\"]", [{return, list}, trim]),
%%	    [_,Key] = re:split(Flag,"[- ]",[{return,list}, trim]),
%%	    unzip(IoDev, [{Key, Value} | Acc])
%%    end.    
