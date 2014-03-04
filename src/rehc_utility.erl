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
%% @doc Tedious & repetitive tasks 
-module(rehc_utility).
-author('zgbjgg@gmail.com').

-export([make_node/2, unmake_node/1, mnesia_support/2, no_empty_lists/1, get_value/2, 
	 get_values/2, perform/1, formatted_date/0, formatted_time/0, get_env_api/0,
	 to_string/1, encode_mode_back/1]).

%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Make node, from "name_node", "hostname" to 'name_node@hostname'.
%%
%% @spec make_node(Host :: string(), Name :: string()) -> atom() 
%% @end
%%--------------------------------------------------------------------
-spec make_node(Host :: string(), Name :: string()) -> atom().
make_node(Host, Name) ->
    Node = Name++"@"++Host,
    list_to_atom(Node).

%%--------------------------------------------------------------------
%% @doc
%% Unmake node, from 'name_node@hostname' to "name_node", "hostname".
%%
%% @spec unmake_node(Node :: atom() | list()) -> {Host :: string(), Name :: string()} 
%% @end
%%--------------------------------------------------------------------
-spec unmake_node(Node :: atom() | list()) -> {Host :: string(), Name :: string()}.
unmake_node(Node) when is_atom(Node) ->
    unmake_node(atom_to_list(Node));
unmake_node(Node) when is_list(Node) ->
    [Name,Host]=re:split(Node, "[@]", [{return, list}, trim]),
    {Host, Name}.

%%--------------------------------------------------------------------
%% @doc
%% No empty elements (lists) in a list
%%
%% @spec no_empty_lists(List :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec no_empty_lists(List :: list()) -> list().
no_empty_lists([])      -> [];
no_empty_lists([[]| T]) -> no_empty_lists(T);
no_empty_lists([H | T]) -> [H] ++ no_empty_lists(T).

%%--------------------------------------------------------------------
%% @doc
%% Get value of a proplist
%%
%% @spec get_value(Proplist :: list(), AnyKey :: term()) -> undefined | term() 
%% @end
%%--------------------------------------------------------------------
-spec get_value(Proplist :: list(), AnyKey :: term()) -> undefined | term().
get_value(Proplist, AnyKey) ->
    V = [ Value || {Key, Value} <- Proplist, Key == AnyKey ],
    case V of [] -> undefined; _ -> [Vv] = V, Vv end.

%%--------------------------------------------------------------------
%% @doc
%% Get values of a proplist
%%
%% @spec get_values(Proplist :: list(), Keys :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec get_values(Proplist :: list(), Keys :: list()) -> list().
get_values(Proplist, Keys) ->
    [ get_value(Proplist, Key) || Key <- Keys ].

%%--------------------------------------------------------------------
%% @doc
%% Enable or disabled mnesia support
%%
%% @spec mnesia_support(enabled | disabled, RehcCore :: list()) -> {ok, enabled} | {ok, disabled}
%% @end
%%--------------------------------------------------------------------
-spec mnesia_support(enabled | disabled, RehcCore :: list()) -> {ok, enabled} | {ok, disabled}.
mnesia_support(disabled, _) ->
    {ok, disabled};
mnesia_support(enabled, RehcCore) ->
    ok = mnesia:start(),
    rehc_sync:init(RehcCore),
    {ok, enabled}.

%%--------------------------------------------------------------------
%% @doc
%% Perform actions: stop, kill and start the application
%%
%% @spec perform(A :: list()) -> {ok, {A :: list(), Performed :: list()}}
%% @end
%%--------------------------------------------------------------------
-spec perform(A :: list()) -> {ok, {A :: list(), Performed :: list()}}.
perform(A) ->
    [ NodeFlag, StartFlag, StopFlag,
      AppFlag ] = get_values(A, [ hostname, start, stop, app ]),
    Cmms = [ StopFlag, "killall -9 "++ AppFlag, StartFlag],
    {ok, Cluster} = application:get_env(rehc, cluster),
    Slave = ?MODULE:get_value(Cluster, slave),
    Performed = [ rpc:call(?MODULE:make_node(NodeFlag, Slave), os, cmd, [Cmm]) || Cmm <- Cmms ],
    {ok, {A, Performed}}.

%%--------------------------------------------------------------------
%% @doc
%% Date format string
%%
%% @spec formatted_date() -> string()
%% @end
%%--------------------------------------------------------------------
-spec formatted_date() -> string().
formatted_date() ->
    {Year,Month,Day} = date(),
    lists:flatten(io_lib:fwrite("~4..0w-~2..0w-~2..0w", [Year,Month,Day])).

%%--------------------------------------------------------------------
%% @doc
%% Time format string
%%
%% @spec formatted_time() -> string()
%% @end
%%--------------------------------------------------------------------
-spec formatted_time() -> string().
formatted_time() ->
    {Hour, Minute, Second} = time(),
    lists:flatten(io_lib:fwrite("~2..0w:~2..0w:~2..0w", [Hour,Minute,Second])).

%%--------------------------------------------------------------------
%% @doc
%% Get environment variables for reply on the headers of http request.
%%
%% @spec get_env_api() -> list().
%% @end
%%--------------------------------------------------------------------
-spec get_env_api() -> list().
get_env_api() ->
    {ok, Api} = application:get_env(rehc, rehc_web_api),
    Headers = proplists:get_value(allow_headers, Api),
    Methods = proplists:get_value(allow_methods, Api),
    Origin = proplists:get_value(allow_origin, Api),
    [Origin, Methods, Headers].

%% @spec to_string([] | term() | list()) -> string()
%% @doc Make a string to send via WS service.
-spec to_string([] | term() | list()) -> string().
to_string([])                    -> "";
to_string(H) when not is_list(H) -> [H];
to_string([H | T])               ->
    to_string(H) ++ to_string(T).

%% @spec encode_mode_back({'json' | 'xml' | 'x-erlang-term', term()} | term()) -> term()
%% @doc Encodes the back response as env
-spec encode_mode_back({'json' | 'xml' | 'x-erlang-term', term()} | term()) -> term().
encode_mode_back({'json', ToEncode}) 	      ->
    Decoder = mochijson2:encoder([{utf8, true}]),
    Json = Decoder(ToEncode),
    to_string(Json);
encode_mode_back({'xml', ToEncode})	      ->
    ToEncode;
encode_mode_back({'x-erlang-term', ToEncode}) ->
    term_to_binary(ToEncode);
encode_mode_back(ToEncode) 	     ->
    {ok, Core} = application:get_env(rehc, rehc_core),
    ModeBack = proplists:get_value(mode_back, Core, 'json'),
    encode_mode_back({ModeBack, ToEncode}).
