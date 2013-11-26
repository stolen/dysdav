-module(dys_reg).
-export([reg/1, unreg/1, locate/1]).

-define(STORAGE, dys_storage_mongo).

reg(StoreID) when is_binary(StoreID) ->
  ?STORAGE:store(reg, StoreID, term_to_binary(self())).

unreg(StoreID) when is_binary(StoreID) ->
  ?STORAGE:delete(reg, StoreID, term_to_binary(self())).

locate(StoreID) when is_binary(StoreID) ->
  case ?STORAGE:fetch(reg, StoreID) of
    undefined ->
      undefined;
    BinPid ->
      Pid = binary_to_term(BinPid),
      true = is_pid(Pid),
      Pid
  end.
