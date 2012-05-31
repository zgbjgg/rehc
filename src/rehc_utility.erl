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
-export([make_node/2, unmake_node/1, parse_request/1, retrieve_pid/1,
	 no_empty_lists/1, get_value/2, get_values/2, status/1]).

%%--------------------------------------------------------------------
%% Make node
make_node(Host, Name) ->
    Node = Name++"@"++Host,
    list_to_atom(Node).

%%--------------------------------------------------------------------
%% Unmake node
unmake_node(Node) when is_atom(Node) ->
    unmake_node(atom_to_list(Node));
unmake_node(Node) when is_list(Node) ->
    [Name,Host]=re:split(Node, "[@]", [{return, list}, trim]),
    {Host, Name}.

%%--------------------------------------------------------------------
%% Parse simple command
parse_request(Req) ->
    R = re:split(Req, "[\n]", [{return, list}, trim]),
    R1 = re:split(R, "[ ]", [{return, list}, trim]),
    no_empty_lists(R1).


%%--------------------------------------------------------------------
%% Command to get pid on OS based on UNIX
retrieve_pid(App) ->
    Cmd = "ps -eo pid,args | grep "++App++" | grep -v grep | cut -c1-6",
    list_to_atom(Cmd).

%%--------------------------------------------------------------------
%% No empty elements in a list
no_empty_lists([])      -> [];
no_empty_lists([[]| T]) -> no_empty_lists(T);
no_empty_lists([H | T]) -> [H] ++ no_empty_lists(T).

%%--------------------------------------------------------------------
%% Retrieve values of a proplist
get_value(Proplist, AnyKey) ->
    [ Value || {Key, Value} <- Proplist, Key == AnyKey ].

%%--------------------------------------------------------------------
%% Return values of a proplist
get_values(Proplist, Keys) ->
    [ proplists:get_value(Key, Proplist) || Key <- Keys ].

%%--------------------------------------------------------------------
%% Status for the application
status(A) ->
    [ TestFlag, FlagInoff, NodeFlag,
      AppFlag ] = rehc_utility:get_values(A,["test","off","node","app"]),
    {ok, State} = rehc_cluster:request(NodeFlag, os, cmd, [TestFlag]),
    case re:run(State, FlagInoff) of
	{match, _} -> {nok, AppFlag};
	nomatch    -> {ok, AppFlag}
    end.
    


