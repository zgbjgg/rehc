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
%% @doc Notify to the master node about cpu statistics, memory consumption and
%%	disk space, by default we monitor every second
-module(rehc_notifier).
-author('zgbjgg@gmail.com').
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {my_master_node=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Master) ->
    gen_server:start(?MODULE, [Master], []).

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
init([Master]) ->
    process_flag(trap_exit, true),
    {ok, #state{my_master_node=Master}, 1000}.

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
handle_call(_Msg, _From, State) ->
    {reply, ok, State, 1000}.

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
    {noreply, State, 1000}.

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
handle_info(timeout, State=#state{my_master_node=Master}) ->
    Mem = rehc_statistics:mem_info(node()),
    Cpu = rehc_statistics:cpu_info(node()),
    Disk = rehc_statistics:disk_info(node()),
    _Ret = erlang:send({'rehc_alarm', Master}, {'HOST-INFO', node(), {mem_alarm(Mem), cpu_alarm(Cpu), disk_alarm(Disk)}}), 
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
terminate(Reason, #state{my_master_node=Master}) ->
    erlang:send({'rehc_ack', Master}, {terminate, node(), Reason, {?MODULE, [Master]}}),
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

disk_alarm(Disk) ->
    disk_alarm(Disk, []).
    
disk_alarm([], Acc) -> Acc;
disk_alarm([{ _FileSystem, [{avail, _Avail}, {used, Used}]}=F | Disk], Acc) when Used>89 ->
    disk_alarm(Disk, [{critical, [F]} | Acc ]);
disk_alarm([{ _FileSystem, [{avail, _Avail}, {used, Used}]}=F | Disk], Acc) when Used>49 ->
    disk_alarm(Disk, [{warning, [F]} | Acc ]);
disk_alarm([{ _FileSystem, [{avail, _Avail}, {used, Used}]}=F | Disk], Acc) when Used<50 ->
    disk_alarm(Disk, [{notice, [F]} | Acc ]).

cpu_alarm({_, _Cpu1, Cpu2, _}=Cpu) ->
    Idle = proplists:get_value(idle, Cpu2),
    cpu_alarm(Cpu, Idle).

cpu_alarm(Cpu, Idle) when Idle<9 ->
    [{critical, Cpu}];
cpu_alarm(Cpu, Idle) when Idle<49 ->
    [{warning, Cpu}];
cpu_alarm(Cpu, Idle) when Idle<100 ->
    [{notice, Cpu}].
   

mem_alarm(Mem) ->
    Total = proplists:get_value(total_memory, Mem),
    Free = proplists:get_value(free_memory, Mem),
    Percent = (100 * Free) / Total, 
    mem_alarm(Percent, Mem).

mem_alarm(Percent, Mem) when Percent>89 ->
    [{critical, Mem}];
mem_alarm(Percent, Mem) when Percent>49 ->
    [{warning, Mem}];
mem_alarm(Percent, Mem) when Percent<50 ->
    [{notice, Mem}].
