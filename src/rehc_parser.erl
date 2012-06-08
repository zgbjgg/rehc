%% ==============================================================================
%
% REHC PARSER	
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
-module(rehc_parser).
-vsn("1.0").
-export([get_config/1, grep_cpu/0]).
-include("rehc.hrl").

%% =============================/ get_config  \===================================
%% Create the configuration, read the files and parse them, for each file, create
%% a new list with the parameters used internally by the system.
%% ===============================================================================
get_config(Dir) ->
    Files = lists:filter(fun(X) -> filelib:is_file(X) end,
			 filelib:wildcard(Dir ++ "/*.rehc")),
    {ok, unzipped_files(Files)}.

%% ========================/ unzipped_files  \===================================
%% Unzipped files values, is a ListOfLists!
%% ==============================================================================
unzipped_files([])             -> [];
unzipped_files([File | Files]) ->
    {ok, IoDev} = file:open(File, [read]),
    [unzip(IoDev, []) | unzipped_files(Files)].
    
%% ==================================/ unzip \===================================
%% Unzip file
%% ==============================================================================
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

%% ===============================/ grep_cpu \===================================
%% Get the line that starts with 'cpu' on file /proc/stat 
%% ==============================================================================
grep_cpu() ->
    {ok, Line} = rehc_utility:rpc('rehc@remote_host', os, cmd,
				  ["cat "++?PROC_STAT++" | grep '^cpu '"]),
    [ L ] = re:split(Line, "[\n]", [{return, list}, trim]),
    [ "cpu", [] | Values ] = re:split(L, "[ ]", [{return, list}, trim]),
    Total = rehc_utility:add_values(Values),
    Idle = list_to_integer(rehc_utility:get_element(Values, 4)),
    {Total, Idle}.





    
