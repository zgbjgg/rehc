%% ==============================================================================
%
% REHC SYNCHRONICITY MNESIA 
%
% Copyright (c) 2012 Jorge Garrido <jorge.garrido@morelosoft.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
% 3. Neither the name of copyright holders nor the names of its
%    contributors may be used to endorse or promote products derived
%    from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%% ===============================================================================
-module(rehc_sync).
-vsn("1.0").
-export([init/1]).
-include("rehc.hrl").

%% ==================================/ init  \===================================
%% Starts the synchronization and prints the info about restored records 
%% ==============================================================================
init(RehcCore) ->
    [ Nodes ] = rehc_utility:get_values(RehcCore, [mnesia_nodes]),
    [ begin
	  Req = tasks(Nodes),
	  Printable = info(Nodes, Req),
	  [ ?LOG_INFO(?RESTORE_DB, [R, N]) || {N, R} <- Printable ]
     end || Node <- Nodes, net_adm:ping(Node) == pong ].

%% ===============================/ tasks \======================================
%% Execute performed tasks to synchronize mnesia database
%% ==============================================================================
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

%% ===============================/ info \ =====================================
%% Print info about restored records
%% =============================================================================
info([], _)                 -> [];
info([ Node | Nodes ], Req) ->
    Fun = fun({N,_}, Acc) -> case N of Node -> Acc + 1; _ -> Acc end end,
    [{Node, lists:foldl(Fun, 0, Req)} | info(Nodes, Req) ].
			  
		  


    
