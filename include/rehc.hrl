%%%-------------------------------------------------------------------
%%% @author Jorge Garrido <george@GeorgeHova.local>
%%% @copyright (C) 2012, Jorge Garrido
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2012 by Jorge Garrido <george@GeorgeHova.local>
%%%-------------------------------------------------------------------

-define(ERROR_LOG(App), error_logger:error_msg("~p down!~n",[App])).
-define(ATTEMPT_LOG(App), error_logger:info_msg("~p: trying restarted ....",[App])).
-define(RESTORE_LOG(App), error_logger:info_msg("~p restarted     [OK]",[App])).
