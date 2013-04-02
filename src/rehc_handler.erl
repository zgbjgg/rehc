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
%% @doc Handle HTTP or WS requests, http dispatch to other module, self() is for
%%	websocket connections
-module(rehc_handler).
-author('zgbjgg@gmail.com').
-export([handle_http/1, handle_websocket/1]).

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handle http request.
%%
%% @spec handle_http(Req :: term()) -> term()
%% @end
%%--------------------------------------------------------------------
-spec handle_http(Req :: term()) -> term().
handle_http(Req) ->
    Args = {Req:get(body), Req:get(headers)},
    Env = rehc_utility:get_env_api(), 
    rehc_w_api:api(Req, Req:get(method), Req:resource([urldecode]), Args, Env).

%%--------------------------------------------------------------------
%% @doc
%% Handle websocket request.
%%
%% @spec handle_websocket(Ws :: term()) -> term()
%% @end
%%--------------------------------------------------------------------
-spec handle_websocket(Ws :: term()) -> term().
handle_websocket(Ws) ->
    receive
        {browser, [$s, $b, $c, $- | Node]}      ->
            {ok, Reference} = rehc_alarm:subscribe(self(), list_to_atom(Node)),            
	    Ws:send([erlang:ref_to_list(Reference)]),
            handle_websocket(Ws);
        {browser, [$u, $s, $b, $c, $- | Node]} ->
            ok;
        {host, Msg}                            ->
            Ws:send([lists:flatten(io_lib:format("~p", [Msg]))]),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    end.

%%%===================================================================
%%% PRIVATE API
%%%===================================================================
