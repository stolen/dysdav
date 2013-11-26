-module(dys_storage_mongo).

% Run startup sequence
-export([setup/0]).

% Stupid API
-export([store/3, fetch/2, delete/2, delete/3]).


setup() ->
  application:start(emongo),
  emongo:add_pool(dys_meta, "localhost", 27017, "dysdav_meta", 10).


% Map pair type to corresponding collection
type_to_collection(inode) -> "inodes";
type_to_collection(reg) -> "regs";
type_to_collection(current) -> "current".


store(Type, Key, Value) when is_atom(Type), is_binary(Key), is_binary(Value) ->
  ok = emongo:update(dys_meta, type_to_collection(Type),
                [{<<"_id">>, Key}],
                [{<<"_id">>, Key}, {<<"value">>, {binary, 0, Value}}],
                true).

fetch(Type, Key) when is_atom(Type), is_binary(Key) ->
  FindResult = emongo:find_one(dys_meta, type_to_collection(Type),
                               [{<<"_id">>, Key}]),
  case FindResult of
    [] ->
      undefined;
    [Obj] ->
      {binary, _, Value} = proplists:get_value(<<"value">>, Obj),
      Value
  end.

delete(Type, Key) when is_atom(Type), is_binary(Key) ->
  ok = emongo:delete(dys_meta, type_to_collection(Type),
                     [{<<"_id">>, Key}]).

delete(Type, Key, Value) when is_atom(Type), is_binary(Key), is_binary(Value) ->
  ok = emongo:delete(dys_meta, type_to_collection(Type),
                     [{<<"_id">>, Key}, {<<"value">>, {binary, 0, Value}}]).

