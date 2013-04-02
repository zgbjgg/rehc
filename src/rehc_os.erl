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
-module(rehc_os).

-export([nodes_load/1, smart_startup/3]).

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Loads all the modules in the list in all nodes (remote hosts).
%%
%% @spec nodes_load(Modules :: list()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec nodes_load(Modules :: list()) -> ok.
nodes_load([])                   ->
    ok;
nodes_load([ Module | Modules ]) ->
    abcast = c:nl(Module),
    nodes_load(Modules). 

%%--------------------------------------------------------------------
%% @doc
%% Start some applications or any module function (gen_server) on the
%% remote host.
%%
%% @spec smart_startup(Node :: atom(), Applications :: list(), Acc :: list()) -> Acc :: list
%% @end
%%--------------------------------------------------------------------
-spec smart_startup(Node :: atom(), Applications :: list(), Acc :: list()) -> Acc :: list().
smart_startup(_Node, [], Acc)                           ->
    Acc;
smart_startup(Node, [{application, App} | Xs], Acc)     ->
    Sisg = rpc:call(Node, application, start, [App]),
    smart_startup(Node, Xs, [Sisg | Acc]);
smart_startup(Node, [{module, Module, Fun, Args} | Xs], Acc) ->
    Sisg = rpc:call(Node, Module, Fun, Args),
    smart_startup(Node, Xs, [Sisg | Acc]).

%%%===================================================================
%%% PRIVATE API
%%%===================================================================
