%%%-------------------------------------------------------------------
%%% @author Jorge Garrido <george@GeorgeHova.local>
%%% @copyright (C) 2012, Jorge Garrido
%%% @doc
%%%    Supervisor for application 'REHC'
%%% @end
%%% Created : 28 May 2012 by Jorge Garrido <george@GeorgeHova.local>
%%%-------------------------------------------------------------------
-module(rehc_sup).
-vsn("1.0").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},
    {ok, { SupFlags, [?CHILD(rehc_cluster, worker),
		      ?CHILD(rehc_monitor, worker),
		      ?CHILD(rehc_support, worker)]} }.

