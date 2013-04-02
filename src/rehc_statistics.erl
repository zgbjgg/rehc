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
-module(rehc_statistics).

-export([net_info/2, mem_info/1, os_info/1,
	 cpu_info/1, total_processes/1, 
	 disk_info/1]).

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get version, type and env vars of the remote host
%%
%% @spec os_info(Node :: atom()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec os_info(Node :: atom()) -> list().
os_info(Node) ->
    [{version, rpc:call(Node, os, version, [])},
     {type, rpc:call(Node, os, type, [])},
     {env, rpc:call(Node, os, getenv, [])}].

%%--------------------------------------------------------------------
%% @doc
%% Get network info of the remote host (provide the link like:
%% "eth0", "wlan0", etc...)
%%
%% @spec net_info(Node :: atom(), Link :: string()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec net_info(Node :: atom(), Link :: string()) -> list().
net_info(Node, Link) ->
    {ok, Net} = rpc:call(Node, inet, getifaddrs, []),
    proplists:get_value(Link, Net).

%%--------------------------------------------------------------------
%% @doc
%% Get memory info of the remote host (avail, free, swap, caché, etc...)
%%
%% @spec mem_info(Node :: atom()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec mem_info(Node :: atom()) -> list().
mem_info(Node) ->
    rpc:call(Node, memsup, get_system_memory_data, []).    

%%--------------------------------------------------------------------
%% @doc
%% Get cpu info of the remote host (user, nice, idle, sys, etc...)
%%
%% @spec cpu_info(Node :: atom()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec cpu_info(Node :: atom()) -> list().
cpu_info(Node) ->
    rpc:call(Node, cpu_sup, util, [[detailed]]).

%%--------------------------------------------------------------------
%% @doc
%% Get number of total processes of the remote host
%%
%% @spec total_processes(Node :: atom()) -> integer()
%% @end
%%--------------------------------------------------------------------
-spec total_processes(Node :: atom()) -> integer().
total_processes(Node) ->
    rpc:call(Node, cpu_sup, nprocs, []).

%%--------------------------------------------------------------------
%% @doc
%% Get disk info of the remote host
%%
%% @spec disk_info(Node :: atom()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec disk_info(Node :: atom()) -> list().
disk_info(Node) ->
    Dev = rpc:call(Node, disksup, get_disk_data, []),
    [ {FileSystem, [{avail, Avail}, {used, Used}] }|| {FileSystem, Avail, Used} <- Dev].

%%%===================================================================
%%% PRIVATE API
%%%===================================================================
