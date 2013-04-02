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
-module(rehc_alarm).

-vsn("1.0").

-behaviour(gen_server).

-export([start_link/0, subscribe/2, unsubscribe/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("rehc.hrl").

-record(state, {subscriptors=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Subscribes a process (PID) to send info about vmstats. 
%%
%% @spec subscribe(Pid :: pid(), Node :: atom()) -> {ok, Ref :: reference()}
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Pid :: pid(), Node :: atom()) -> {ok, Ref :: reference()}.
subscribe(Pid, Node) ->
    gen_server:call(?MODULE, {subscribe, Pid, Node}).

%%--------------------------------------------------------------------
%% @doc
%% Unsubscribes a process (PID) to send info about vmstats. 
%%
%% @spec unsubscribe(Reference :: reference()) -> {ok, unsubscribed} 
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Reference :: reference()) -> {ok, unsubscribed}.
unsubscribe(Reference) ->
    gen_server:call(?MODULE, {unsubscribe, Reference}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

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
    {ok, #state{subscriptors=[]}}.

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
handle_call({subscribe, Pid, Node}, _From, #state{subscriptors=S}) ->
    Reference = erlang:monitor(process, Pid),
    ?LOG_INFO("Subscribe ~p", [Reference]),
    {reply, {ok, Reference}, #state{subscriptors=[{Reference, {Pid, Node}}|S]}};
handle_call({unsubscribe, Reference}, _From,  #state{subscriptors=S}) ->
    {reply, {ok, unsubscribe}, #state{subscriptors=[ {Ref, Pid} || {Ref, Pid} <- S, Ref =/= Reference]}}.

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
handle_cast(_Msg, State)  ->
    {noreply, State}.

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
handle_info({'HOST-INFO', Node, {Mem, Cpu, Disk}}, State=#state{subscriptors=S}) ->
    MemStats = [ Stats || {Stats, _} <- Mem ],
    CpuStats = [ Stats || {Stats, _} <- Cpu ],
    DiskStats = [ {FS, Stats} || {Stats, [{FS, _}]} <- Disk ],
    % ?LOG_INFO("Node ~p:  Mem ~p, Cpu ~p, Disk ~p", [Node, MemStats, CpuStats, DiskStats]),
    ok = subscriptors_notify(S, Node, {Mem, Cpu, Disk}),
    {noreply, State};
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, #state{subscriptors=S}) ->
    ?LOG_INFO("Unsubscribe ~p", [MonitorRef]),
    {noreply, #state{subscriptors=[ {Ref, Pid} || {Ref, Pid} <- S, Ref =/= MonitorRef]}}.

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

%%--------------------------------------------------------------------
%% @doc
%% Notify to the subscriptors processes (PIDS) about vmstats. 
%%
%% @spec subscriptors_notify(Processes :: list(), Node :: atom(), { Mem :: list(), Cpu :: list(), Disk :: list()}) -> ok 
%% @end
%%--------------------------------------------------------------------
-spec subscriptors_notify(Processes :: list(), Node :: atom(), { Mem :: list(), Cpu :: list(), Disk :: list()}) -> ok.
subscriptors_notify([], _Node, _Vmstats)                          ->
    ok;
subscriptors_notify([ {_Ref, {Pid, SubNode}} | Ss ], Node, {Mem, Cpu, Disk}) when Node =:= SubNode ->
    Pid ! {host, {{mem, Mem}, {cpu, Cpu}, {disk, Disk}}},
    subscriptors_notify(Ss, Node, {Mem, Cpu, Disk});
subscriptors_notify([ _ | Ss ], Node, Vmstats) ->
    subscriptors_notify(Ss, Node, Vmstats).
