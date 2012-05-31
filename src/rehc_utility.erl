%%%-------------------------------------------------------------------
%%% @author Jorge Garrido <george@GeorgeHova.local>
%%% @copyright (C) 2012, Jorge Garrido
%%% @doc
%%%
%%% @end
%%% Created : 28 May 2012 by Jorge Garrido <george@GeorgeHova.local>
%%%-------------------------------------------------------------------
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
    


