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
-module(rehc_notifier).

-vsn("1.0").

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
