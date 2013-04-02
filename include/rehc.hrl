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

%============================/ Logs \=============================================
-define(LOG_INFO(Log, Args), io:format("(~p) ~s :[info] "++Log++"\n", [self(), ?DATE_LOG | Args])).
-define(LOG_ERROR(Log, Args), io:format("(~p) ~s :[error] "++Log++"\n",[self(), ?DATE_LOG | Args])).
-define(LOG_WARN(Log, Args), io:format("(~p) ~s :[warning] "++Log++"\n",[self(), ?DATE_LOG | Args])).
-define(LOG_DEBUG(Log, Args), io:format("(~p) ~s :[debug] "++Log++"\n",[self(), ?DATE_LOG | Args])).

%============================/ External Logs \====================================
-define(DATE_LOG, rehc_utility:formatted_date()++" "++rehc_utility:formatted_time()).
-define(INFO_MSG(Msg, Args), io:format(Msg, Args)).

%=============================/ String Messages \=================================
-define(DOWN_APP, "~p down! ~n").
-define(ATTEMPT, "~p: trying restart at ~p ......~n").
-define(RESTORE, "~p: restart [OK] ~n").
-define(RESTORE_DB, "~p restored of remote node ~p ~n").
-define(RESTORE_SLAVE, "Restarted node '~p' at ~p~n").
-define(MNESIA, "~p mnesia sync ~n").
-define(TERMINATE, "Filed server support for reason:~n ~p ~n").
-define(START_SLAVE, "Started slave '~p' at ~p ~n").
-define(WSTART_SLAVE, "Couldn't start slave '~p' at ~p ~n").
-define(DISCONNECTED_SLAVE, "Node '~p' disconnected at ~p ~n").
-define(APP, "Node ~p ==>  [ app ~p :  status : ~p ] ~n").
-define(LA, " ==~s== ~n CPU: ~p % \t ~s \t ~s ~n ~n").

%===========================/ Mail Messages \====================================
-define(MAIL(Date, App, Ip, Log), Date++" \n RHEC MAIL \n \n \t Application:"++
	App++" \n \t Ip: "++Ip++" \n \t Reason:"++Log++" \n \n \n 2012 - REHC -").

%===========================/ Mnesia Parameters \================================
-define(CHANGE_CONFIG, [extra_db_nodes, [node()]]).
-define(SYSTEM_INFO, [tables]).
-define(ADD_TABLE_COPY(Tb, Tp), [Tb, node(), Tp]).

%=================================/ Nodes \======================================
-define(COOKIE(Args), "-setcookie "++proplists:get_value(cookie, Args)).
-define(SNAME(Args), proplists:get_value(slave, Args)).

%================================/ LOAD AVERAGE \================================
-define(PROC_MEMINFO, ["cat /proc/meminfo | grep '^MemTotal: '",
		       "cat /proc/meminfo | grep '^MemFree: '"]).
-define(PROC_STAT, "cat /proc/stat | grep '^cpu '").

