-record(account,{
  accountId,
  balance=0 :: float()
  }).

-record(history,{
  accountId,
  money :: float(),
%%  TODO don't work
  type:: start | deposit | draw,
%% TODO   duplicate bag - not support in mesia
  time
}).