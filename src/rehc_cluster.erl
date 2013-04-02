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

%%--------------------------------------------------------------------
%% @doc
%% Add node to cluster, the added node is connected with this master 
%% node.
%%
%% @spec add_node(Host :: string(), Name :: string(), Ip :: string()) -> {ok, connected} | {nok, could_not_connect}
%% @end
%%--------------------------------------------------------------------
-spec add_node(Host :: string(), Name :: string(), Ip :: string()) -> {ok, connected} | {nok, could_not_connect}.
add_node(Host, Name, Ip) ->
    gen_server:call(?MODULE, {adding_node, Host, Name, Ip}).

%%--------------------------------------------------------------------
%% @doc
%% Get all connected nodes (remote hosts).
%%
%% @spec get_nodes() -> list()
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> list().
get_nodes() ->
    gen_server:call(?MODULE, getting_nodes).

%%--------------------------------------------------------------------
%% @doc
%% Stops the gen server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
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
    Nodes = [begin
  		 case slave:start(Host, ?SNAME(Config), ?COOKIE(Config)) of
		     {ok, Node} ->
		         ?LOG_INFO(?START_SLAVE, [Node, Ip]),
			 {Node, Ip};
		     {error, timeout} ->
			 ?LOG_WARN(?WSTART_SLAVE, [Host, Ip])
		  end
  	     end || {Host, Ip, _} <- rehc_utility:get_value(Config, servers)],  
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{nodes=[ Node || Node <- Nodes, ok =/= Node ]}}.

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
    {ok, Node} = slave:start(Host, Name, ?COOKIE(Config)),
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
    {Host, Name} = rehc_utility:unmake_node(NodeDown),
    [ Ip ] = [ Ip || {Node, Ip} <- Nodes, Node =:= NodeDown],
    ?LOG_WARN(?DISCONNECTED_SLAVE, [NodeDown, Ip]),
    Connected = [ X || {Node, _}=X <- Nodes, Node =/= NodeDown ],
    {ok, Config} = application:get_env(rehc, cluster),
    {ok, _NodeUp} = slave:start(Host, Name, ?COOKIE(Config)),
    {noreply, State#state{nodes=Connected}};
handle_info({nodeup, NodeUp}, State=#state{nodes=Nodes})     ->
    {HostUp, _Name} = rehc_utility:unmake_node(NodeUp),
    {ok, Config} = application:get_env(rehc, cluster),
    [ IpUp ] = [ Ip || {Host, Ip, _} <- rehc_utility:get_value(Config, servers), Host =:= HostUp],
    ?LOG_INFO(?RESTORE_SLAVE, [NodeUp, IpUp]),
    ok = rehc_os:nodes_load([rehc_statistics, rehc_monitor, rehc_notifier]),
    {ok, RehcCore} = application:get_env(rehc, rehc_core),
    {ok, Cluster} = application:get_env(rehc, cluster),
    RehcConfigDir = rehc_utility:get_value(RehcCore, rehc_config_dir), 
    {ok, Apps} = rehc_parser:get_config(RehcConfigDir),	
    Req = rehc_utility:get_value(Cluster, require_apps),
    Run = rehc_os:smart_startup(NodeUp, [{module, rehc_monitor, start_link, [node(), Apps]},
					 {module, rehc_notifier, start_link, [node()]} | Req], []),
    ?LOG_INFO("Remote startups: ~p", [Run]), 
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

%%--------------------------------------------------------------------
%% @doc
%% Writes .ssh/config file with info about connections to the remote
%% hosts that rehc'll control.
%%
%% @spec ssh_config(Config :: list()) -> list()
%% @end
%%--------------------------------------------------------------------
-spec ssh_config(Config :: list()) -> list().
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
