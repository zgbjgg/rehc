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
-module(rehc_monitor).
-vsn("1.0").
-behaviour(gen_server).

-export([start_link/2, get_apps_mon/0, restore/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {apps_mon=[], my_master_node=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get apps that is being monitored remotely. 
%%
%% @spec get_apps_mon() -> {ok, Apps :: list()}
%% @end
%%--------------------------------------------------------------------
-spec get_apps_mon() -> {ok, Apps :: list()}.
get_apps_mon() ->
    gen_server:call(?MODULE, getting_apps_mon).

%%--------------------------------------------------------------------
%% @doc
%% Restore an app that was down, but now is running, after reload it.
%%
%% @spec restore(A :: string(), App :: list()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restore(A :: string(), App :: list()) -> ok.
restore(A, App) ->
    gen_server:cast(?MODULE, {restoring, A, App}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Master :: atom(), Apps :: list()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Master, Apps) ->
    gen_server:start(?MODULE, [Master, Apps], []).

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
init([Master, Apps]) ->
    process_flag(trap_exit, true),
    {ok, #state{apps_mon=Apps, my_master_node=Master}, 1000}.

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
handle_call(getting_apps_mon, _From, State=#state{apps_mon=Apps, my_master_node=_Master}) ->
    {reply, {ok, Apps}, State, 1000}.

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
handle_cast({restoring, App, _Appname}, State=#state{apps_mon=Apps, my_master_node=Master})  ->
    {noreply, State#state{apps_mon=[App | Apps], my_master_node=Master}, 1000}.

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
handle_info({ok, 'UP', App}, #state{apps_mon=Apps, my_master_node=Master}) ->
    {noreply, #state{apps_mon=[App | Apps], my_master_node=Master}, 1000};
handle_info(timeout, #state{apps_mon=Apps, my_master_node=Master}) ->
    NewApps = [ App || App <- Apps, ok =:= av_api(App, Master) ],
    {noreply, #state{apps_mon=NewApps, my_master_node=Master}, 1000};
handle_info({'EXIT', _Port ,normal}, State) ->
    {noreply, State, 1000};
handle_info(Info, State=#state{apps_mon=_Apps, my_master_node=Master}) ->
    erlang:send({'rehc_ack', Master}, {message, 'UNHANDLED', Info, node()}),
    {noreply, State, 1000}.	    

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
terminate(Reason, #state{apps_mon=_Apps, my_master_node=Master}) ->
    erlang:send({'rehc_ack', Master}, {terminate, node(), Reason}),
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

av_api(App, Master) ->
    TestFlag = proplists:get_value(test, App),
    InOff = proplists:get_value(off, App),
    Port = open_port({spawn, TestFlag},[binary]),
    Incomming = receive {Port, {data, In}} -> In end,
    [Vs, <<>>] = binary:split(Incomming, [<<"\n">>], []), 
    case list_to_binary(InOff) of
	Vs -> 
	    erlang:send({'rehc_ack', Master}, {alert, 'DOWN', App, node()}), 
	    {alert, 'DOWN'};
	_Any   ->
	    ok
    end.	
