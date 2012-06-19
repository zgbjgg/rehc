%% ==============================================================================
%
% REHC LOAD AVERAGE
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
-module(rehc_loadavg).
-vsn("1.0").
-include("rehc.hrl").
-export([init/1]).

%% =================================/ init \=====================================
%% Spawn a new child to monitor the status of remmote host (cpu, mem)
%% ==============================================================================
init(Node) ->
    spawn(fun() -> loop(Node, 0, 0) end).

%% =================================/ loop \=====================================
%% Simple loop to refresh info every second
%% ==============================================================================
loop(Node, PrevTotal, PrevIdle) ->
    receive
	start ->
	    {Total, Idle} = proc_stat(Node),
	    loop(Node, Total, Idle);
	stop  ->
	    ok
    after 1000 ->
	    {Total, Idle} = proc_stat(Node),
	    Usage = calc_cpu(Total, Idle, PrevTotal, PrevIdle),
	    {MemTotal, MemFree} = proc_meminfo(Node),
	    ?INFO_MSG(?LA, [?DATE_LOG, Usage, MemTotal, MemFree]),
	    loop(Node, Total, Idle)
    end.

%% ===============================/ proc_stat \==================================
%% Get the line that starts with 'cpu' on file /proc/stat and return some values
%% ==============================================================================
proc_stat(Node) ->
    {ok, Line} = rehc_utility:rpc(Node, os, cmd,[?PROC_STAT]),
    [ L ] = re:split(Line, "[\n]", [{return, list}, trim]),
    [ "cpu", [] | Values ] = re:split(L, "[ ]", [{return, list}, trim]),
    Total = rehc_utility:add_values(Values),
    Idle = list_to_integer(rehc_utility:get_element(Values, 4)),
    {Total, Idle}.

%% ===========================/ proc_meminfo \===================================
%% Parse file /proc/meminfo 
%% ==============================================================================
proc_meminfo(Node) ->
    [ Total, Free ] = ?PROC_MEMINFO,
    {ok, LineT} = rehc_utility:rpc(Node, os, cmd, [ Total ]),
    {ok, LineF} = rehc_utility:rpc(Node, os, cmd, [ Free ]),
    [ T ] = re:split(LineT, "[\n]", [{return, list}, trim]),
    [ F ] = re:split(LineF, "[\n]", [{return, list}, trim]),
    {T, F}.

%% =============================/ calc_cpu \====================================
%% Calculate used percent of CPU 
%% =============================================================================
calc_cpu(Total, Idle, PrevTotal, PrevIdle) ->
    DiffIdle = Idle - PrevIdle,
    DiffTotal = Total - PrevTotal,
    (1000 * (DiffTotal - DiffIdle) / DiffTotal + 5 ) / 10.
