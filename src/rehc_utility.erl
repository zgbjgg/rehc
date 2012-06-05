%% ==============================================================================
%
% REHC UTILITY	
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
-module(rehc_utility).
-vsn("1.0").
-export([make_node/2, unmake_node/1, parse_request/1, mnesia_support/2,
	 no_empty_lists/1, get_value/2, get_values/2, status/1,
	 rpc/4, perform/1]).

%% ===========================/ make_node \======================================
%% Make node, it looks like: 'name_node@hostname'
%% ==============================================================================
make_node(Host, Name) ->
    Node = Name++"@"++Host,
    list_to_atom(Node).

%% ==========================/ unmake_node \=====================================
%% Unmake node, it looks like: "name_node@hostname"
%% ==============================================================================
unmake_node(Node) when is_atom(Node) ->
    unmake_node(atom_to_list(Node));
unmake_node(Node) when is_list(Node) ->
    [Name,Host]=re:split(Node, "[@]", [{return, list}, trim]),
    {Host, Name}.

%% =========================/ parse_request \====================================
%% Parse the response of a command executed on remote host
%% ==============================================================================
parse_request(Req) ->
    R = re:split(Req, "[\n]", [{return, list}, trim]),
    R1 = re:split(R, "[ ]", [{return, list}, trim]),
    one_list(no_empty_lists(R1)).

%% =========================/ no_empty_lists \===================================
%% No empty elements (lists) in a list
%% ==============================================================================
no_empty_lists([])      -> [];
no_empty_lists([[]| T]) -> no_empty_lists(T);
no_empty_lists([H | T]) -> [H] ++ no_empty_lists(T).

%% ===============================/ one_list \===================================
%% one list represented as string  
%% ==============================================================================
one_list([])      -> [];
one_list([H | T]) -> H ++ (T).

%% ==============================/ get_value \===================================
%% Get value of a proplist
%% ==============================================================================
get_value(Proplist, AnyKey) ->
    V = [ Value || {Key, Value} <- Proplist, Key == AnyKey ],
    case V of [] -> undefined; _ -> [Vv] = V, Vv end.

%% ==============================/ get_value \===================================
%% Get values of a proplist
%% ==============================================================================
get_values(Proplist, Keys) ->
    [ get_value(Proplist, Key) || Key <- Keys ].

%% =================================/ status \===================================
%% Status of application, possible reponses are: {ok, App} | {nok, App}
%% ==============================================================================
status(A) ->
    [ TestFlag, FlagInoff, NodeFlag,
      AppFlag ] = rehc_utility:get_values(A,["test","off","node","app"]),
    {ok, State} = rpc(NodeFlag, os, cmd, [TestFlag]),
    case re:run(State, FlagInoff) of
	{match, _} -> {nok, AppFlag};
	nomatch    -> {ok, AppFlag}
    end.
%% =================================/ status \===================================
%% Remote procedure call to node
%% ==============================================================================
rpc(Node, M, F, A) when is_atom(Node) ->
    case rpc:call(Node, M, F, A) of
	{badrpc, _}=X -> X;
	X             -> {ok, X}
    end;
rpc(Host, M, F, A)                    ->
    Node = make_node(Host, "rehc"),
    rpc(Node, M, F, A).

%% ================================/ perform \===================================
%% Perform actions to stop, kill and start the application
%% ==============================================================================
perform(A) ->
    [ NodeFlag, StartFlag, StopFlag,
      AppFlag ] = get_values(A, ["node", "start", "stop", "app"]),
    Cmms = [ StopFlag, "killall -9 "++ AppFlag, StartFlag],
    Performed = [ rpc(NodeFlag, os, cmd, [Cmm]) || Cmm <- Cmms ],
    {ok, {A, Performed}}.

%% ===============================/ mnesia_support \============================
%% Enable or disabled mnesia support
%% =============================================================================
mnesia_support(disabled, _) ->
    {ok, disabled};
mnesia_support(enabled, RehcCore) ->
    ok = mnesia:start(),
    rehc_sync:init(RehcCore),
    {ok, enabled}.
