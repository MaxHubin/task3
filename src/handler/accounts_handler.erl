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
-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
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

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

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
    {undefined, Req2} ->
      {true, Req2, index};
    {AccountId, _Req2} ->
      case bank:find_account(AccountId) of
        {atomic, []} -> {false, Req, []};
        {atomic, History} -> {true, Req, History}
      end
  end.

delete_resource(Req, index) ->
  invalid_account(Req, []);

delete_resource(Req, []) ->
  invalid_account(Req, []);

delete_resource(Req, _Array) ->
  {AccountId,_Req}=cowboy_req:binding(accountid, Req),
  bank:delete_account(AccountId),
  {true, Req, _Array}.



handle_in(Req, State) ->
  {ok, ReqBody, _Req3} = cowboy_req:body(Req),
  Answer = case cowboy_req:method(Req) of
             {<<"POST">>,_Req} -> in_post(ReqBody);
             {<<"PUT">>,_Req} ->
               {AccountId,_Req}=cowboy_req:binding(accountid, Req),
               in_put(ReqBody, AccountId)
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
  Body = jiffy:encode(
    [[AccountId, Balance] || {_Type, AccountId, Balance} <- Accounts]
  ),
  {Body, Req, index};

handle_req(Req, []) ->
  invalid_account(Req, []);

handle_req(Req, History) ->
  Body = jiffy:encode(
    [[TypeTransaction, Money, Timestamp] || {_Type, _AccountId, Money, TypeTransaction, Timestamp} <- History]
  ),
  {Body, Req, History}.


wrong_amount(Req, State) ->
  Req2 = cowboy_req:set_resp_body(["{\"code\": ", ?WRONG_AMOUNT, "}"], Req),
  {false, Req2, State}.

invalid_account(Req, State) ->
  Req2 = cowboy_req:set_resp_body(["{\"code\": ", ?INVALID_ACCOUNT, "}"], Req),
  {false, Req2, State}.

rest_terminate(_Req, _State) ->
  ok.