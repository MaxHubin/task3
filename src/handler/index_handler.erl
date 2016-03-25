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
  {ok, Req2} = cowboy_req:reply(200, [], "index", Req),
  {ok, Req2, {}}.