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
-module(rehc_command_line_tool).
-author('zgbjgg@gmail.com').
-vsn("1.0").
-include("rehc.hrl").
-export([cluster/0, failure_apps/0, add_slave/4]).

cluster() ->
    Cluster = rehc_cluster:get_nodes(),
    io:format("~79..#s~n", [""]),
    io:format("Slaves Monitor Nodes AGREE ~n"),
    io:format("~79..#s~n~n", [""]),
    io:format("~-30s ~-15s \n", ["Hostname", "IP"]),
    io:format("~79..-s~n", [""]),
    [ io:format("~-30s ~-15s \n", [atom_to_list(Hostname), Ip]) || {Hostname, Ip} <- Cluster ].

failure_apps() ->
    {ok, AppsFailure} = rehc_ack:get_failure_apps(),
    io:format("~79..#s~n", [""]),
    io:format("Applications Monitored FAILURE ~n"),
    io:format("~79..#s~n", [""]),
    io:format("~-30s ~-30s ~s\n", ["App", "Hostname", "Pinging"]),
    io:format("~79..-s~n", [""]),
    [ io:format("~-30s ~-15s ~p\n", [proplists:get_value(app, App), proplists:get_value(hostname, App), proplists:get_value(ping, App)]) || App <- AppsFailure ].

add_slave(Hostname, Ip, User, Port) ->
    ExitStatus = rehc_cluster:add_node(Hostname, Ip, User, list_to_integer(Port)),
    io:format("~-30p \n", ["Add Slave"]),
    io:format("~79..-s~n", [""]),
    io:format("~-30p \n", [ExitStatus]).
