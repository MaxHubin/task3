%%%-------------------------------------------------------------------
%%% @author maxsim
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2016 23:18
%%%-------------------------------------------------------------------
-module(edit_handler).
-author("maxsim").

%% API
-export([
  init/2
]).

init(Req, _Opts) ->
  ID =  cowboy_req:binding(accountid, Req),
  {ok, HTML} = edit_tpl:render([{username, ID}]),
  {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
  {ok, Req2, {}}.
