%% ==============================================================================
%
% REHC INCLUDE FILE	
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

%============================/ Logs \=============================================
-define(LOG_INFO(Log, Args), error_logger:info_msg("[REHC INFO]~n"++Log, Args)).
-define(LOG_ERROR(Log, Args), error_logger:error_msg("[REHC_ERROR]~n"++Log,Args)).
-define(LOG_WARN(Log, Args), error_logger:warning_msg("[REHC WARNING]~n"++Log,Args)).
-define(LOG_DEBUG(Log, Args), error_logger:info_msg("[REHC DEBUG]~n"++Log,Args)).

%=============================/ String Messages \=================================
-define(DOWN_APP, "~p down! ~n").
-define(ATTEMPT, "~p: trying restart at ~p ......~n").
-define(RESTORE, "~p: restart [OK] ~n").
-define(RESTORE_DB, "~p restored of remote node ~p ~n").
-define(RESTORE_SLAVE, "Restarted node '~p' at ~p~n").
-define(MNESIA, "~p mnesia sync ~n").
-define(TERMINATE, "Filed server support for reason:~n ~p ~n").
-define(START_SLAVE, "Started slave '~p' at ~p ~n").
-define(DISCONNECTED_SLAVE, "Node '~p' disconnected at ~p ~n").

%===========================/ Mnesia Parameters \================================
-define(CHANGE_CONFIG, [extra_db_nodes, [node()]]).
-define(SYSTEM_INFO, [tables]).
-define(ADD_TABLE_COPY(Tb, Tp), [Tb, node(), Tp]).

%=================================/ Nodes \======================================
-define(COOKIE(Args), "-setcookie "++proplists:get_value(cookie, Args)).
-define(SNAME(Args), proplists:get_value(slave, Args)).

%================================/ LOAD AVERAGE \================================
-define(PROC_MEMINFO, "/proc/meminfo").
-define(PROC_STAT, "/proc/stat").

