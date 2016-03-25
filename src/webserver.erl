-module(webserver).

%% API
-export([
  start/0,
  stop/0
]).

-define(APPS, [crypto, ranch, cowlib, cowboy,mnesia,jiffy, webserver]).
-include("include/models.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  ok = ensure_started(?APPS),
  mnesia:create_table(account,   [{attributes, record_info(fields, account)}, {disc_copies, [node()] },{type, set}]),
  mnesia:create_table(history,   [{attributes, record_info(fields, history)}, {disc_copies, [node()] },{type, bag}]).

stop() ->
  ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
  case application:start(App) of
    ok -> ensure_started(Apps);
    {error, {already_started, App}} -> ensure_started(Apps)
  end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
  application:stop(App),
  stop_apps(Apps).