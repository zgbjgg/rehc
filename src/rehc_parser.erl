%%%-------------------------------------------------------------------
%%% @author Jorge Garrido <george@GeorgeHova.local>
%%% @copyright (C) 2012, Jorge Garrido
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2012 by Jorge Garrido <george@GeorgeHova.local>
%%%-------------------------------------------------------------------
-module(rehc_parser).
-vsn("1.0").
-export([get_config/1]).

%%--------------------------------------------------------------------
%% create the configuration, read the files and parse them, for each
%% file, create a new list with the parameters used internally by
%% the system
get_config(Dir) ->
    Files = lists:filter(fun(X) -> filelib:is_file(X) end,
			 filelib:wildcard(Dir ++ "/*.rehc")),
    {ok, unzipped_files(Files)}.

%%---------------------------------------------------------------------
%% Unzipped files values, is a ListOfLists!
unzipped_files([])             -> [];
unzipped_files([File | Files]) ->
    {ok, IoDev} = file:open(File, [read]),
    [unzip(IoDev, []) | unzipped_files(Files)].
    
%%---------------------------------------------------------------------
%% Unzip file
unzip(IoDev, Acc) ->
    case io:get_line(IoDev, "") of
	eof    ->
	    file:close(IoDev),
	    Acc;
	Line   ->
	    [Flag,Value,_]=re:split(Line, "[\"\"]", [{return, list}, trim]),
	    [_,Key] = re:split(Flag,"[- ]",[{return,list}, trim]),
	    unzip(IoDev, [{Key, Value} | Acc])
    end.
	    
