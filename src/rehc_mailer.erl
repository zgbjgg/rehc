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
-module(rehc_mailer).
-vsn("1.0").
-include("rehc.hrl").
-export([send/3]).

%% =================================/ send \=====================================
%% Send mail to notify about application state
%% ==============================================================================
send(A, App, Reason) ->
    Host = rehc_utility:get_value(A, node),
    NodeIn = rehc_utility:make_node(Host, "rehc"),
    Nodes = rehc_cluster:get_nodes(),
    [ Ip ] = [ Ip || {Node, Ip} <- Nodes, Node == NodeIn ],
    Msg = ?MAIL(?DATE_LOG, App, Ip, Reason),
    [ os:cmd("echo -e \""++Msg++"\"| mail -s \"Rehc\" "++Email) ||
	Email <- emails() ].

%% ===============================/ emails \=====================================
%% Get list of emails configured to receive notifications 
%% ==============================================================================
emails() ->
    {ok, RehcMail} = application:get_env(rehc, rehc_email), 
    rehc_utility:get_value(RehcMail, mail).
