%%%-------------------------------------------------------------------
%%% @author maxsim
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2016 23:18
%%%-------------------------------------------------------------------
-module(not_found_handler).
-author("maxsim").

%% API
-export([
  init/2
]).

init(Req, _Opts) ->
  {ok, Req2} = cowboy_req:reply(400, [], "ERROR", Req),
  {ok, Req2, {}}.
