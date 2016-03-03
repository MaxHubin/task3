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
            {"/accounts/[:accountid]",[{accountid, int}], edit_handler, []},
            {"/",[], index_handler, []},
            {'_', not_found_handler, []}
        ]}
    ]),

    Port = 8008,
    {ok, _} = cowboy:start_http(http_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),

    webserver_sup:start_link().

stop(_State) ->
    ok.
