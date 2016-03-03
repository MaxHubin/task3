%%%-------------------------------------------------------------------
%%% @author maxsim
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2016 23:18
%%%-------------------------------------------------------------------
-module(index_handler).
-author("maxsim").

%% API
-export([
  init/2
]).

init(Req, _Opts) ->
  {ok, HTML} = index_tpl:render([{accounts, [[1,2],[3,4],[5,6],[7,8]]}]),
  {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
  {ok, Req2, {}}.