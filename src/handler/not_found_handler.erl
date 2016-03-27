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
  init/3,
  terminate/3,
  handle/2
]).


init(_Type, Req, _Opts) ->
  {ok, Req, {}}.

handle(Req, State) ->
  URL = element(1, cowboy_req:url(Req)),
  {ok, HTML} = '404_tpl':render([{url, URL}]),
  {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
  {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
  ok.