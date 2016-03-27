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
  URL = cowboy_req:url(Req),
  {ok, HTML} = '404_tpl':render([{url, URL}]),
  {ok, cowboy_req:reply(404, [], HTML, Req), {}}.
