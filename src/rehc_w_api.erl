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
%% @doc Dispatch the incoming web request (API)
-module(rehc_w_api).
-author('zgbjgg@gmail.com').

-export([api/5]).

-include("rehc_api.hrl").

%%--------------------------------------------------------------------
%% @doc
%% WEB API :(
%%
%% @spec api(Req :: term(), Method :: atom(), list(), 
%%	     Args :: tuple(), list()) -> term().
%% @end
%%--------------------------------------------------------------------
-spec api(Req :: term(), Method :: atom(), list(), Args :: tuple(), list()) -> term().
api(Req, 'GET', ["cluster", "hosts"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    VMstats = rehc_cluster:get_nodes(),
    Decoder = mochijson2:encoder([{utf8, true}]),
    JSON = Decoder({struct, [ {Host, list_to_atom(Ip)} || {Host, Ip}<- VMstats]}),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("application/json"), JSON);
api(Req, 'GET', ["host", "stats"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    [{"node", Node}] = Req:parse_qs(),
    VMstats = rehc_statistics:os_info(list_to_atom(Node)),
    Decoder = mochijson2:encoder([{utf8, true}]),
    JSON = Decoder({struct, force(proplists:delete(env, VMstats)) ++ 
			[ {env, [ list_to_atom(I) || I <- In ]} || {env, In} <- VMstats ]}),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("application/json"), JSON);
api(Req, 'GET', ["host", "net"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    ParseQs = Req:parse_qs(),
    Node = proplists:get_value("node", ParseQs),
    Link = proplists:get_value("link", ParseQs),
    VMstats = rehc_statistics:net_info(list_to_atom(Node), Link),
    Decoder = mochijson2:encoder([{utf8, true}]),
    JSON = Decoder({struct, [ {Key, force_string(In, [])} || {Key, In}<- force(VMstats) ]}),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("application/json"), JSON);
api(Req, _Method, _Uri, {_Body, _Headers}, [Origin, Methods, Headers]) ->
    Req:respond(404, ?HEADERS(Origin, Methods, Headers), ""). 

%%--------------------------------------------------------------------
%% @doc
%% Forces to tuples elements into lists.
%%
%% @spec force(list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec force(list()) -> list().
force([{ Key, Value} | Rest]) when is_tuple(Value) ->
    [ { Key, tuple_to_list(Value)} | force(Rest) ];
force([ Pairs | Rest ]) ->
    [ Pairs | force(Rest)];
force([]) ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% Forces to integer elements  into lists.
%%
%% @spec force_string(list(), Acc :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec force_string(list(), Acc :: list()) -> list().
force_string([ E | []], Acc) when is_integer(E) ->
    Final = Acc ++ integer_to_list(E),
    list_to_atom(Final);     
force_string([ E | Es ], Acc) when is_integer(E) ->
    force_string(Es, Acc ++ integer_to_list(E) ++ ",");
force_string([ E | Es ], Acc) ->
    force_string(Es, [ E | Acc]);
force_string([], Acc) ->
    Acc.
