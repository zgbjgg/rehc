%% ==============================================================================
%
% REHC CLUSTER	
%
% Copyright (c) 2012 Jorge Garrido <jorge.garrido@morelosoft.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
% 3. Neither the name of copyright holders nor the names of its
%    contributors may be used to endorse or promote products derived
%    from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%% ===============================================================================
-module(rehc_cluster).
-vsn("1.0").
-behaviour(gen_server).
-include("rehc.hrl").
%% API
-export([start_link/0, add_node/3, stop/0, get_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {nodes=[]}).

%%%===================================================================
%%% API
%%%===================================================================

add_node(Host, Name, Ip) ->
    gen_server:call(?MODULE, {adding_node, Host, Name, Ip}).

get_nodes() ->
    gen_server:call(?MODULE, getting_nodes).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Config} = application:get_env(rehc, cluster),
    [] = ssh_config(Config),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

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
init([Config]) ->
    process_flag(trap_exit, true),
    Opt = ?COOKIE(Config),
    Sname = ?SNAME(Config),
    Nodes = [begin
  		 {ok, Node} = slave:start(Host, Sname, Opt),
		 ?LOG_INFO(?START_SLAVE, [Node, Ip]),
  		 {Node, Ip}
  	     end || {Host, Ip, _} <- rehc_utility:get_value(Config, servers)],
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{nodes=Nodes}}.

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
handle_call({adding_node, Host, Name, Ip}, _From, State=#state{nodes=N}) ->
    {ok, Config} = application:get_env(rehc, cluster),
    Opt = ?COOKIE(Config),
    {ok, Node} = slave:start(Host, Name, Opt),
    {NewState, Reply} =
	case net_adm:ping(Node) of
	    pong -> {State#state{nodes=[{Node,Ip}]++N}, {ok, connected}};
	    _    -> {State#state{nodes=[]++N}, {nok, could_not_connect}}
	end,
    {reply, Reply, NewState};
handle_call(getting_nodes, _From, State=#state{nodes=N})                 ->
    {reply, N, State};
handle_call(stop, _From, State)                                          ->
    {stop, normal, ok, State}.

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
handle_cast(_Msg, State) ->
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
handle_info({nodedown, NodeDown}, State=#state{nodes=Nodes}) ->
    [ Ip ] = [ Ip || {Node, Ip} <- Nodes, Node =:= NodeDown],
    ?LOG_WARN(?DISCONNECTED_SLAVE, [NodeDown, Ip]),
    Connected = [ X || {Node, _}=X <- Nodes, Node =/= NodeDown ],
    {Host, Name} = rehc_utility:unmake_node(NodeDown),
    {ok, Config} = application:get_env(rehc, cluster),
    {ok, _NodeUp} = slave:start(Host, Name, ?COOKIE(Config)),
    {noreply, State#state{nodes=Connected}};
handle_info({nodeup, NodeUp}, State=#state{nodes=Nodes})     ->
    {HostUp, _} = rehc_utility:unmake_node(NodeUp),
    {ok, Config} = application:get_env(rehc, cluster),
    [ IpUp ] = [ Ip || {Host, Ip, _} <- rehc_utility:get_value(Config, servers), Host =:= HostUp],
    ?LOG_INFO(?RESTORE_SLAVE, [NodeUp, IpUp]),
    {noreply, State#state{nodes=Nodes ++ [{NodeUp, IpUp}]}}.

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
ssh_config(Config) ->
    [HomeDir]=re:split(os:cmd("echo $HOME"),"[\n]",[{return,list}, trim]),
    os:cmd("rm -rf " ++ HomeDir ++ "/.ssh/config"),
    {ok, IoDev} = file:open(HomeDir ++ "/.ssh/config", [write, append]),
    Servers = rehc_utility:get_value(Config, servers),
    [ begin
	  io:format(IoDev, "Host\t~s~n\tHostname ~s~n\tPort ~p~n\tUser ~s~n",
		    [HostName, HostName, Port, User])
      end || {HostName, _, [{user, User}, {port, Port}]} <- Servers ],
    os:cmd("chmod 600 " ++ HomeDir ++ "/.ssh/config").
