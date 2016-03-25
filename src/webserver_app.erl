-module(webserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/accounts/[:accountid]",[{accountid, int}], accounts_handler, []},
            {"/",[], index_handler, []},
            {'_', not_found_handler, []}
        ]}
    ]),

    Port = 8008,
    {ok, _} = cowboy:start_http(http, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),

    bank:start_link(),
    webserver_sup:start_link().

stop(_State) ->
    ok.
