%%%-------------------------------------------------------------------
%%% @author maxsim
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2016 03:37
%%%-------------------------------------------------------------------
-module(bank).
-author("maxsim").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  uniq_id/0,
  time_millis/0,
  code_change/3]).

-export([create_account/1,
  delete_account/1,
  deposit_amount/2,
  find_account/1,
  find_accounts/0,
  with_draw_amount/2]).

-include("include/models.hrl").

find_account(AccountId) -> gen_server:call(?MODULE, {find, AccountId}).
find_accounts() -> gen_server:call(?MODULE, {findAll}).
create_account(InitialBalance) -> gen_server:call(?MODULE, {create, InitialBalance}).
delete_account(AccountId) -> gen_server:call(?MODULE, {delete, AccountId}).
deposit_amount(AccountId, Amount) -> gen_server:call(?MODULE, {deposite, AccountId, Amount}).
with_draw_amount(AccountId, Amount) -> gen_server:call(?MODULE, {draw, AccountId, Amount}).

%% gen_server callbacks


-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({find, AccountId}, _From, Tab) ->
  History = mnesia:transaction(fun() -> mnesia:read({history, AccountId}) end),
  {reply, History, Tab};

handle_call({findAll}, _From, Tab) ->
  CatchAll = [{'_', [], ['$_']}],
  Accounts = mnesia:transaction(fun() -> mnesia:select(account, CatchAll) end),
  {reply, Accounts, Tab};

handle_call({create, InitialBalance}, _From, Tab) ->
  case is_number(InitialBalance) of
    false -> {reply, {false, wrong_amount}, Tab};
    true ->
      UniqId = uniq_id(),
      AccountRecord = #account{accountId = UniqId, balance = InitialBalance},
      HistoryRecord = #history{accountId = UniqId, money = InitialBalance, type = start, time = time_millis()},
      {atomic, Result} =mnesia:transaction(
        fun() ->
          mnesia:write(AccountRecord),
          mnesia:write(HistoryRecord),
          UniqId
        end
      ),
      {reply, Result, Tab}
  end;


handle_call({delete, AccountId}, _From, Tab) ->
  {atomic, Result}=mnesia:transaction(
    fun() ->
      mnesia:delete({account, AccountId}),
      mnesia:delete({history, AccountId}),
      ok
    end
  ),
  {reply, Result, Tab};

handle_call({deposite, AccountId, Amount}, _From, Tab) ->
  {atomic, Result} = case (Amount =< 0) or (is_number(Amount) =:= false) of
             true -> {reply, {false, wrong_amount}, Tab};
             false -> insert_history(AccountId, Amount, deposite)
           end,
  {reply, Result, Tab};

handle_call({draw, AccountId, Amount}, _From, Tab) ->
  {atomic, Result} = case (Amount >= 0) or (is_number(Amount) =:= false) of
             true -> {false, wrong_amount};
             false -> insert_history(AccountId, Amount, draw)
           end,
  {reply, Result, Tab}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


insert_history(AccountId, Amount, Type) ->
  mnesia:transaction(
    fun() ->
      case (mnesia:read({account, AccountId})) of
        [] -> {false, invalid_account};
        Array ->
          [{_type, AccountId, Balance}] = Array,
          NewBalance = Balance + Amount,
          AccountRecord = #account{accountId = AccountId, balance = NewBalance},
          HistoryRecord = #history{
            accountId = AccountId,
            money = Amount,
            type = Type,
            time = time_millis()
          },
          mnesia:write(AccountRecord),
          mnesia:write(HistoryRecord)
      end
    end
  ).


time_millis() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  (MegaSecs * 1000000 + Secs) * 1000 + erlang:trunc(MicroSecs / 1000).

uniq_id() ->
  time_millis() * 100 + random:uniform(99).