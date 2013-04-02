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
-module(rehc_handler).

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
    rehc_wapi:api(Req, Req:get(method), Req:resource([urldecode]), Args, Env).

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
