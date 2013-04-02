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
-module(rehc_ack).

-vsn("1.0").

-behaviour(gen_server).

-include("rehc.hrl").

-export([start_link/0, get_failure_apps/0, remove_failure_app/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {failure_apps=[], timeout}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get apps that is not running on any remote host, this apps crashes
%% for some reason.
%%
%% @spec get_failure_apps() -> {ok, List :: list()}
%% @end
%%--------------------------------------------------------------------
-spec get_failure_apps() -> {ok, List :: list()}.
get_failure_apps() ->
    gen_server:call(?MODULE, getting_failure_apps).

%%--------------------------------------------------------------------
%% @doc
%% Remove the app because now is running on any remote host, it will 
%% be useful to keep this gen_server with low latency.
%%
%% @spec remove_failure_app(App :: string(), Appname :: list()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec remove_failure_app(App :: string(), Appname :: list()) -> ok.
remove_failure_app(App, Appname) ->
    gen_server:cast(?MODULE, {removing_failure_apps, App, Appname}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, RehcCore} = application:get_env(rehc, rehc_core),
    {ok, Cluster} = application:get_env(rehc, cluster),
    [ Timeout, RehcConfigDir ] = rehc_utility:get_values(RehcCore, [max_time_restart, 
								    rehc_config_dir]),
    {ok, Config} = rehc_parser:get_config(RehcConfigDir), 
    Req = rehc_utility:get_value(Cluster, require_apps), 
    Nodes = rehc_cluster:get_nodes(),
    ok = rehc_os:nodes_load([rehc_statistics, rehc_monitor, rehc_notifier]),
    [ begin 
	  Run = rehc_os:smart_startup(Node, [{module, rehc_monitor, start_link, [node(), Config]}, 
					     {module, rehc_notifier, start_link, [node()]} | Req], []), 
          ?LOG_INFO("Remote startups: ~p", [Run]) 
        end || {Node, _} <- Nodes ],
    {ok, #state{failure_apps=[], timeout=Timeout}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(getting_failure_apps, _From, State=#state{failure_apps=Apps, timeout=_Timeout}) ->
    {reply, {ok, Apps}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({removing_failure_apps, AppFailed, _Appname}, State=#state{failure_apps=Apps, timeout=_Timeout})  ->
    {noreply, State#state{failure_apps=[ App || App <- Apps, App =/= AppFailed]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({message, 'UNHANDLED', Info, Node}, State) ->
    ?LOG_INFO("~p on ~p", [{message, 'UNHANDLED', Info}, Node]),
    {noreply, State};
handle_info({terminate, Node, Reason}, State) ->
    ?LOG_INFO("~p on ~p", [{terminate, Reason}, Node]),
    {noreply, State};
handle_info({alert, 'DOWN', App, Node}, #state{failure_apps=Apps, timeout=Timeout}) ->
    {ok, _} = rehc_utility:perform(App),
    _TimerRef = erlang:send_after(Timeout, 'rehc_ack', {ok, 'EVAL-EP', App, Node}),
    ?LOG_WARN("~p on ~p", [{alert, 'DOWN', {app, proplists:get_value(app, App)}}, Node]),
    {noreply, #state{failure_apps=[App | Apps], timeout=Timeout}};
handle_info({ok, 'EVAL-EP', App, Node}, #state{failure_apps=Apps, timeout=Timeout}) ->
    ?LOG_DEBUG("~p on ~p", [{ok, 'EVAL-EP', {app, proplists:get_value(app, App)}}, Node]),
    erlang:send({'rehc_monitor', Node}, {ok, 'UP', App}), 	    
    {noreply, #state{failure_apps=[ AppF || AppF <- Apps, App =/= Apps], timeout=Timeout}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
