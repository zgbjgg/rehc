%%%-------------------------------------------------------------------
%%% @author Jorge Garrido <george@GeorgeHova.local>
%%% @copyright (C) 2012, Jorge Garrido
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2012 by Jorge Garrido <george@GeorgeHova.local>
%%%-------------------------------------------------------------------
-module(rehc_monitor).
-vsn("1.0").
-behaviour(gen_server).
-include("rehc.hrl").

%% API
-export([start_link/0, get_state/0, restore/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {appmon=[]}).

%%%===================================================================
%%% API
%%%===================================================================

get_state() ->
    gen_server:call(?MODULE, getting_state).

restore(A, App) ->
    gen_server:call(?MODULE, {restoring, A, App}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, RehcCore} = application:get_env(rehc, rehc_core),
    [RehcConfigDir] = rehc_utility:get_values(RehcCore, [rehc_config_dir]),
    {ok, Config} = rehc_parser:get_config(RehcConfigDir),
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
    {ok, #state{appmon=Config}, 1000}.

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
handle_call({restoring, A, App}, _From, State=#state{appmon=H})  ->
    ?RESTORE_LOG(App),
    {reply, ok, State#state{appmon=H++[A]}, 1000};
handle_call(getting_state, _From, State=#state{appmon=H}) ->
    {reply, H, State, 1000}.

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
handle_info(timeout, State=#state{appmon=H}) ->
    St = [ begin
	       {Y,App} = rehc_utility:status(A),
	       case Y of
		   ok -> A;
		   nok ->
		       ?ERROR_LOG(App),
		       ok = rehc_support:add_app(A),
		       []
	       end
	   end || A <- H ],
    NewState = rehc_utility:no_empty_lists(St),
    {noreply, State#state{appmon=NewState}, 1000}.
	    

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

	    
