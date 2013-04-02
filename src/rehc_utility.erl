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
-module(rehc_utility).

-vsn("1.0").

-export([make_node/2, unmake_node/1, mnesia_support/2, no_empty_lists/1, get_value/2, 
	 get_values/2, perform/1, formatted_date/0, formatted_time/0, get_env_api/0]).

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Make node, from "name_node", "hostname" to 'name_node@hostname'.
%%
%% @spec make_node(Host :: string(), Name :: string()) -> atom() 
%% @end
%%--------------------------------------------------------------------
-spec make_node(Host :: string(), Name :: string()) -> atom().
make_node(Host, Name) ->
    Node = Name++"@"++Host,
    list_to_atom(Node).

%%--------------------------------------------------------------------
%% @doc
%% Unmake node, from 'name_node@hostname' to "name_node", "hostname".
%%
%% @spec unmake_node(Node :: atom() | list()) -> {Host :: string(), Name :: string()} 
%% @end
%%--------------------------------------------------------------------
-spec unmake_node(Node :: atom() | list()) -> {Host :: string(), Name :: string()}.
unmake_node(Node) when is_atom(Node) ->
    unmake_node(atom_to_list(Node));
unmake_node(Node) when is_list(Node) ->
    [Name,Host]=re:split(Node, "[@]", [{return, list}, trim]),
    {Host, Name}.

%%--------------------------------------------------------------------
%% @doc
%% No empty elements (lists) in a list
%%
%% @spec no_empty_lists(List :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec no_empty_lists(List :: list()) -> list().
no_empty_lists([])      -> [];
no_empty_lists([[]| T]) -> no_empty_lists(T);
no_empty_lists([H | T]) -> [H] ++ no_empty_lists(T).

%%--------------------------------------------------------------------
%% @doc
%% Get value of a proplist
%%
%% @spec get_value(Proplist :: list(), AnyKey :: term()) -> undefined | term() 
%% @end
%%--------------------------------------------------------------------
-spec get_value(Proplist :: list(), AnyKey :: term()) -> undefined | term().
get_value(Proplist, AnyKey) ->
    V = [ Value || {Key, Value} <- Proplist, Key == AnyKey ],
    case V of [] -> undefined; _ -> [Vv] = V, Vv end.

%%--------------------------------------------------------------------
%% @doc
%% Get values of a proplist
%%
%% @spec get_values(Proplist :: list(), Keys :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec get_values(Proplist :: list(), Keys :: list()) -> list().
get_values(Proplist, Keys) ->
    [ get_value(Proplist, Key) || Key <- Keys ].

%%--------------------------------------------------------------------
%% @doc
%% Enable or disabled mnesia support
%%
%% @spec mnesia_support(enabled | disabled, RehcCore :: list()) -> {ok, enabled} | {ok, disabled}
%% @end
%%--------------------------------------------------------------------
-spec mnesia_support(enabled | disabled, RehcCore :: list()) -> {ok, enabled} | {ok, disabled}.
mnesia_support(disabled, _) ->
    {ok, disabled};
mnesia_support(enabled, RehcCore) ->
    ok = mnesia:start(),
    rehc_sync:init(RehcCore),
    {ok, enabled}.

%%--------------------------------------------------------------------
%% @doc
%% Perform actions: stop, kill and start the application
%%
%% @spec perform(A :: list()) -> {ok, {A :: list(), Performed :: list()}}
%% @end
%%--------------------------------------------------------------------
-spec perform(A :: list()) -> {ok, {A :: list(), Performed :: list()}}.
perform(A) ->
    [ NodeFlag, StartFlag, StopFlag,
      AppFlag ] = get_values(A, [ node, start, stop, app ]),
    Cmms = [ StopFlag, "killall -9 "++ AppFlag, StartFlag],
    {ok, Cluster} = application:get_env(rehc, cluster),
    Slave = ?MODULE:get_value(Cluster, slave),
    Performed = [ rpc:call(?MODULE:make_node(NodeFlag, Slave), os, cmd, [Cmm]) || Cmm <- Cmms ],
    {ok, {A, Performed}}.

%%--------------------------------------------------------------------
%% @doc
%% Date format string
%%
%% @spec formatted_date() -> string()
%% @end
%%--------------------------------------------------------------------
-spec formatted_date() -> string().
formatted_date() ->
    {Year,Month,Day} = date(),
    lists:flatten(io_lib:fwrite("~4..0w-~2..0w-~2..0w", [Year,Month,Day])).

%%--------------------------------------------------------------------
%% @doc
%% Time format string
%%
%% @spec formatted_time() -> string()
%% @end
%%--------------------------------------------------------------------
-spec formatted_time() -> string().
formatted_time() ->
    {Hour, Minute, Second} = time(),
    lists:flatten(io_lib:fwrite("~2..0w:~2..0w:~2..0w", [Hour,Minute,Second])).

%%--------------------------------------------------------------------
%% @doc
%% Get environment variables for reply on the headers of http request.
%%
%% @spec get_env_api() -> list().
%% @end
%%--------------------------------------------------------------------
-spec get_env_api() -> list().
get_env_api() ->
    {ok, Api} = application:get_env(rehc, rehc_web_api),
    Headers = proplists:get_value(allow_headers, Api),
    Methods = proplists:get_value(allow_methods, Api),
    Origin = proplists:get_value(allow_origin, Api),
    [Origin, Methods, Headers].
