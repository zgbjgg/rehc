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
-module(rehc_sync).

-vsn("1.0").

-export([init/1]).

-include("rehc.hrl").

%%%===================================================================
%%% PUBLIC API
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @doc
%% Starts the synchronization and prints the info about restored 
%% records.
%%
%% @spec init(RehcCore :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec init(RehcCore :: list()) -> list().
init(RehcCore) ->
    [ Nodes ] = rehc_utility:get_values(RehcCore, [mnesia_nodes]),
    [ begin
	  Req = tasks(Nodes),
	  Printable = info(Nodes, Req),
	  [ ?LOG_INFO(?RESTORE_DB, [R, N]) || {N, R} <- Printable ]
     end || Node <- Nodes, net_adm:ping(Node) == pong ].


%%%===================================================================
%%% PRIVATE API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Execute performed tasks to synchronize mnesia database
%%
%% @spec tasks(Nodes :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec tasks(Nodes :: list()) -> list().
tasks([])               -> [];
tasks([ Node | Nodes ]) ->
    {ok,_} = rehc_utility:rpc(Node,mnesia,change_config,?CHANGE_CONFIG),
    {ok,NodeTables}=rehc_utility:rpc(Node,mnesia,system_info,?SYSTEM_INFO),
    [ begin
	  Res = {atomic, ok},
	  {ok,Res}=rehc_utility:rpc(Node,mnesia,add_table_copy,
					?ADD_TABLE_COPY(Table,ram_copies)),
	  {Node, Table}
      end || Table <- NodeTables, Table =/= schema ] ++ tasks(Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Print info about restored records
%%
%% @spec info(Nodes :: list(), Req :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec info(Nodes :: list(), Req :: list()) -> list().
info([], _)                 -> [];
info([ Node | Nodes ], Req) ->
    Fun = fun({N,_}, Acc) -> case N of Node -> Acc + 1; _ -> Acc end end,
    [{Node, lists:foldl(Fun, 0, Req)} | info(Nodes, Req) ].    
