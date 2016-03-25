%%%-------------------------------------------------------------------
%%% @author maxsim
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2016 23:18
%%%-------------------------------------------------------------------
-module(accounts_handler).
-author("maxsim").

%% API
-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([handle_req/2]).
-export([content_types_accepted/2]).
-export([handle_in/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
%%-export([delete_completed/2]).
%%-export([delete_resource/2]).

-define(INVALID_ACCOUNT, "\"INVALID_ACCOUNT\"").
-define(WRONG_AMOUNT, "\"WRONG_AMOUNT\"").
-define(INSUFFICIENT_FUNDS, "\"INSUFFICIENT_FUNDS\"").
-define(INTERNAL_ERROR, "\"INTERNAL_ERROR\"").


init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.


content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_req}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_in}], Req, State}.


resource_exists(Req, _State) ->
  case cowboy_req:binding(accountid, Req) of
    undefined ->
      {true, Req, index};
    AccountId ->
      case bank:find_account(AccountId) of
        {atomic, []} -> {false, Req, index};
        {atomic, History} -> {true, Req, History}
      end
  end.

delete_resource(Req, index) ->
  invalid_account(Req, []);

delete_resource(Req, []) ->
  invalid_account(Req, []);

delete_resource(Req, _Array) ->
  bank:delete_account(cowboy_req:binding(accountid, Req)),
  {true, Req, _Array}.



handle_in(Req, State) ->
  {ok, ReqBody, _Req3} = cowboy_req:body(Req),
  Answer = case cowboy_req:method(Req) of
             <<"POST">> -> in_post(ReqBody);
             <<"PUT">> -> in_put(ReqBody, cowboy_req:binding(accountid, Req))
           end,
  case Answer of
    {false, wrong_amount} ->
      wrong_amount(Req, State);
    {false, invalid_account} ->
      invalid_account(Req, State);
    Answer ->
      JsonAnswer = jiffy:encode({[{status, Answer}]}),
      Req2 = cowboy_req:set_resp_body(JsonAnswer, Req),
      {true, Req2, State}
  end.


in_post(ReqBody) ->
  {[{<<"InitialBalance">>, Balance}]} = jiffy:decode(ReqBody),
  Answer = bank:create_account(Balance),
  Answer.


in_put(ReqBody, AccountId) ->
  {[{<<"Money">>, Balance}]} = jiffy:decode(ReqBody),
  case Balance > 0 of
    true -> bank:deposit_amount(AccountId, Balance);
    false -> bank:with_draw_amount(AccountId, Balance)
  end.


handle_req(Req, index) ->
  {atomic, Accounts} = bank:find_accounts(),
  Req2 = cowboy_req:set_resp_body(jiffy:encode(
    [[AccountId, Balance] || {_Type, AccountId, Balance} <- Accounts]
  ), Req),
  %%TODO ERROR ?
  {ok, cowboy_req:reply(200, Req2), index};

handle_req(Req, []) ->
  invalid_account(Req, []);

handle_req(Req, History) ->
  Req2 = cowboy_req:set_resp_body(jiffy:encode(
    [[TypeTransaction, Money, Timestamp] || {_Type, _AccountId, Money, TypeTransaction, Timestamp} <- History]
  ), Req),
%%TODO ERROR ?
  {ok, cowboy_req:reply(200, Req2), History}.


wrong_amount(Req, State) ->
  Req2 = cowboy_req:set_resp_body(["{\"code\": ", ?WRONG_AMOUNT, "}"], Req),
  {false, Req2, State}.

invalid_account(Req, State) ->
  Req2 = cowboy_req:set_resp_body(["{\"code\": ", ?INVALID_ACCOUNT, "}"], Req),
  {false, Req2, State}.