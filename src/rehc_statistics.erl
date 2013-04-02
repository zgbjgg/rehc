%% _____________________________________________________________________________
%%
%% Copyright (c) 2013 Jorge Garrido <zgbjgg@gmail.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%% 3. Neither the name of copyright holders nor the names of its
%%    contributors may be used to endorse or promote products derived
%%    from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%% PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% _____________________________________________________________________________ 
%%
%% [ zgbjgg ]
%%           
%% @author zgbjgg@gmail.com
%% @copyright 2013 Jorge Garrido
%% @doc Gather info about remote host like network, os, disk, cpu, mem, etc...
-module(rehc_statistics).
-author('zgbjgg@gmail.com').
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
    Network = proplists:get_value(Link, Net),
    net_ipv6(Network).

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

net_ipv6([{addr, Value} | Rest]) ->
    case erlang:tuple_size(Value)>4 of
	true ->
	    [{addr_ipv6, Value} | net_ipv6(Rest)];
   	false ->
	    [{addr, Value} | net_ipv6(Rest)]
    end;
net_ipv6([{netmask, Value} | Rest]) ->
    case erlang:tuple_size(Value)>4 of
        true ->
            [{netmask_ipv6, Value} | net_ipv6(Rest)];
        false ->
            [{netmask, Value} | net_ipv6(Rest)]
    end;
net_ipv6([Pairs | Rest]) ->
    [ Pairs | net_ipv6(Rest)];
net_ipv6([]) ->
    [].

