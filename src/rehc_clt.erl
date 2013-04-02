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
-module(rehc_clt).
-vsn("1.0").
-include("rehc.hrl").
-export([show_apps/0, show_loadavg/1, cluster/0]).

%% ============================/ show_apps \=====================================
%% Tool to show applications state
%% ==============================================================================
show_apps() ->
    Apps = rehc_monitor:get_state(),
    [ begin
	  [ AppFlag, Node ] = rehc_utility:get_values(App, [app,node]),
	  ?INFO_MSG("~p~n", [[ Node, AppFlag, {ok, running} ]])
      end || App <- Apps ].

%% =========================/ show_loadavg \=====================================
%% Tool to show load average for a remote host
%% ==============================================================================
show_loadavg(Ip) ->
    [ Node ] = [ N || {N, NIp} <- rehc_cluster:get_nodes(), NIp ==Ip], 
    Pid = rehc_loadavg:init(Node),
    Pid ! start.

%% =========================/ show_cluster \=====================================
%% Tool to show remote host configured
%% ==============================================================================
cluster() ->    
    Cluster = rehc_cluster:get_nodes(),
    [ begin
	  ?INFO_MSG("{~p, ~s}~n", [Node, Ip])
      end || {Node, Ip} <- Cluster ].

