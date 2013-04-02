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
-module(rehc_wapi).

-export([api/5]).

-include("rehc_api.hrl").

%%--------------------------------------------------------------------
%% @doc
%% WEB API :(
%%
%% @spec api(Req :: term(), Method :: atom(), list(), 
%%	     Args :: tuple(), list()) -> term().
%% @end
%%--------------------------------------------------------------------
-spec api(Req :: term(), Method :: atom(), list(), Args :: tuple(), list()) -> term().
api(Req, 'GET', ["cluster", "hosts"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    VMstats = lists:flatten(io_lib:format("~p", [ rehc_cluster:get_nodes() ])),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("text/plain"), VMstats);
api(Req, 'GET', ["host", "stats"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    [{"node", Node}] = Req:parse_qs(),
    VMstats = lists:flatten(io_lib:format("~p", [ rehc_statistics:os_info(list_to_atom(Node))])),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("text/plain"), VMstats);
api(Req, 'GET', ["host", "net"], {_Body, _Headers}, [Origin, Methods, Headers]) ->
    ParseQs = Req:parse_qs(),
    Node = proplists:get_value("node", ParseQs),
    Link = proplists:get_value("link", ParseQs),
    VMstats = lists:flatten(io_lib:format("~p", [ rehc_statistics:net_info(list_to_atom(Node), Link)])),
    Req:respond(200, ?HEADERS(Origin, Methods, Headers) ++ ?CTYPE("text/plain"), VMstats);
api(Req, _, _, _, [Origin, Methods, Headers]) ->
    Req:respond(404, ?HEADERS(Origin, Methods, Headers), ""). 
