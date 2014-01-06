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
%% @doc Monitor remote applications, every second perform a user command and 
%%	check for its status, if application is down then notify and restart it
-module(rehc_monitor).
-author('zgbjgg@gmail.com').
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
    ok = ping(Apps, Master),
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
terminate(Reason, #state{apps_mon=Apps, my_master_node=Master}) ->
    erlang:send({'rehc_ack', Master}, {terminate, node(), Reason, {?MODULE, [Master, Apps]}}),
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


ping(unset, _Master) 		             -> ok;
ping([{ip, Ip, port, Port} | Rest], Master) ->
    ok = case gen_tcp:connect(Ip, Port, [], 1000) of
             {ok, SocketUp} ->
	         gen_tcp:close(SocketUp);
	     {error, Reason}     ->
		 Net = Ip ++ ":" ++ integer_to_list(Port),
	         erlang:send({'rehc_alarm', Master}, {'PING-INFO', node(), {Net, Reason}}),
    	         ok
    end,
    ping(Rest, Master);
ping([], _Master)			      -> ok;
ping([App | Apps], Master)  	              ->
    Config = proplists:get_value(ping, App, unset),
    ok = ping(Config, Master),
    ping(Apps, Master).    
