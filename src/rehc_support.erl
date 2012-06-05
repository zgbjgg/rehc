%% ==============================================================================
%
% REHC SUPPORT	
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
-module(rehc_support).
-vsn("1.0").
-behaviour(gen_server).
-include("rehc.hrl").
%% API
-export([start_link/0, add_app/1, get_state/0, remove_app/1, next_attempt/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {appmon=[], timeout}).

%%%===================================================================
%%% API
%%%===================================================================

get_state() ->
    gen_server:call(?MODULE, getting_state).

add_app(A) ->
    gen_server:cast(?MODULE, {adding_app,A}).

remove_app(A) ->
    gen_server:cast(?MODULE, {removing_app, A}).

next_attempt(A) ->
    gen_server:cast(?MODULE, {attempting, A}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, RehcCore} = application:get_env(rehc, rehc_core),
    [Max] = rehc_utility:get_values(RehcCore, [max_time_restart]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Max], []).

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
init([Max]) ->
    process_flag(trap_exit,true),
    {ok, RehcCore} = application:get_env(rehc, rehc_core),
    [ Mnesia ] = rehc_utility:get_values(RehcCore, [mnesia]),
    {ok, M} = rehc_utility:mnesia_support(Mnesia, RehcCore),
    ?LOG_INFO(?MNESIA, [ M ]),
    {ok, #state{appmon=[], timeout=Max}, Max}.

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
handle_call(getting_state, _From, State=#state{appmon=D,timeout=_Max}) ->
    {reply, D, State, 1000}.

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
handle_cast({adding_app, A}, State=#state{appmon=D, timeout=Max}) ->
    {ok, _} = rehc_utility:perform(A),
    {noreply, State#state{appmon=[A]++D, timeout=Max}, Max};
handle_cast({attempting, A}, State=#state{appmon=_D, timeout=Max}) ->
    [ AppFlag, Node ] = rehc_utility:get_values(A, ["app", "node"]),
    ?LOG_WARN(?ATTEMPT, [ AppFlag, Node ]),
    {ok, _} = rehc_utility:perform(A),
    {noreply, State, Max};
handle_cast({removing_app, A}, State=#state{appmon=D, timeout=Max}) ->
    NewState = D -- [A],
    {noreply, State#state{appmon=NewState, timeout=Max}, Max}.

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
handle_info(timeout, State=#state{appmon=D, timeout=Max}) ->
    St = [ begin
	       {Y,App} = rehc_utility:status(A),
	       case Y of
		   ok -> ok = rehc_monitor:restore(A, App), [];
		   nok -> ok = rehc_support:next_attempt(A), A
	       end
	   end || A <- D ],
    NewState = rehc_utility:no_empty_lists(St),
    {noreply, State#state{appmon=NewState,timeout=Max}, Max}.

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
terminate(Reason, #state{appmon=D, timeout=_Max}) ->
    ?LOG_ERROR(?TERMINATE, [Reason]),
    [ begin
	  [ AppFlag ] = rehc_utility:get_values(A, ["app"]),
	  ok = rehc_monitor:restore(A, AppFlag)
      end || A <- D ], 
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
