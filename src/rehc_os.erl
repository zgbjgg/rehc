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
%% @doc Actions when slave is started on remote host, this actions must be 
%%	called from another modules which control & manage cluster
-module(rehc_os).
-author('zgbjgg@gmail.com').
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
