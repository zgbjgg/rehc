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
%% @doc Send an alarm of status remote host, like cpu statistics, memory consumption 
%%	and disk space, we manage three alarm levels: 
%%		-notice:   for normal status
%%		-warning:  when not is normal status but not critical
%%		-critical: when status is over 90% in all cases
-module(rehc_alarm).
-author('zgbjgg@gmail.com').
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
handle_info({'PING-INFO', Node, {Net, Info}}, State=#state{subscriptors=S})	 ->
    ok = subscriptors_notify(S, Node, {Net, Info}),
    {noreply, State};
handle_info({'HOST-INFO', Node, {Mem, Cpu, Disk}}, State=#state{subscriptors=S}) ->
    % MemStats = [ Stats || {Stats, _} <- Mem ],
    % CpuStats = [ Stats || {Stats, _} <- Cpu ],
    % DiskStats = [ {FS, Stats} || {Stats, [{FS, _}]} <- Disk ],
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
subscriptors_notify([ {_Ref, {Pid, SubNode}} | Ss ], Node, {Net, Info}) when Node =:= SubNode	   ->
    Back = rehc_utility:encode_mode_back({struct, [{to, list_to_atom(Net)}, {status, Info}]}),
    Pid ! {host, Back},
    subscriptors_notify(Ss, Node, {Net, Info});
subscriptors_notify([ {_Ref, {Pid, SubNode}} | Ss ], Node, {Mem, Cpu, Disk}) when Node =:= SubNode ->
    [ Memory ] = [ {struct, [{stat, Info}, {memory, PList} ]} || {Info, PList} <- Mem ],
    [ Processor ] = [ {struct, [{stat, Info}, {cpu, L1 ++ L2}] }|| {Info, {_, L1, L2, _}} <- Cpu ], 
    
    Disks = [ {struct, [{stat, Info}, {disk, {struct, [ {file_system, FS}|| {FS,_}<- PList]} }]} || {Info, PList} <- Disk ], 
    Back = rehc_utility:encode_mode_back({array, [Memory, Processor, {array, Disks}]}),
    Pid ! {host, Back},
    subscriptors_notify(Ss, Node, {Mem, Cpu, Disk});
subscriptors_notify([ _ | Ss ], Node, Vmstats) ->
    subscriptors_notify(Ss, Node, Vmstats).

