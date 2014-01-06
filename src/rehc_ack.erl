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
%% @doc Receive messages from the client (slave node) 
-module(rehc_ack).
-author('zgbjgg@gmail.com').
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
    {ok, AllApps} = rehc_parser:get_config(RehcConfigDir), 
    Req = rehc_utility:get_value(Cluster, require_apps), 
    Nodes = rehc_cluster:get_nodes(),
    ok = rehc_os:nodes_load([rehc_statistics, rehc_monitor, rehc_notifier]),
    [ begin
	  {HostUp, _Name} = rehc_utility:unmake_node(Node), 
	  Apps = [ App || App <- AllApps, proplists:get_value(hostname, App) =:= HostUp ],
	  Run = rehc_os:smart_startup(Node, [{module, rehc_monitor, start_link, [node(), Apps]}, 
					     {module, rehc_notifier, start_link, [node()]} | Req], []),   
	  ?LOG_INFO("Remoting startups: ~p", [Run]) 
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
handle_info({terminate, Node, Reason, {M, Args}}, State) ->
    ?LOG_INFO("~p on ~p", [{terminate, Reason}, Node]),
    {ok, _} = rpc:call(Node, M, start_link, Args),
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
