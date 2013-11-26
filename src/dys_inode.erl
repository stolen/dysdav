-module(dys_inode).
-behavior(gen_server).

% low-level inode functions. Used for debug, no real need in working system
-export([create/1, dump/1, store_key/1, restore/1]).

% inode server API
-export([start_link/1]).
-export([add_child/2, get_child/2, list_children/1, describe_children/1, update_child/2, del_child/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Fire up supervisor
start_link(StoreKey) ->
  gen_server:start_link(?MODULE, [StoreKey], []).


% Min and max child count. When there are few children, aggregation or local rebalancing fires up.
-define(MIN_CHILDREN, 5).
-define(MAX_CHILDREN, 25).
-define(BAL_CHILDREN, 15). % balanced child count

% Storage adapter
-define(STORAGE, dys_storage_mongo).

% Early version: directory tree is B-tree with metadata cache
-record(inode_v0, {
    % id is persistent, every inode has its own id
    id = undefined, % There should be map of ID -> LatestStoreKey for dir-top inodes
    version = 0, % put millisecond timestamp here
    prev_version = undefined, % pointer to previous version if any
    last_action = undefined, % when there is previous version, it is good to keep change that lead to this one
    children = [], % sorted list of tuples {Name, StorageId, [{Key, Value}] = Metadata}
    left = undefined,  % left, right are store keys of sub-inodes with filenames less and more than local respectively
    right = undefined, % ... and their child count as tuple {FarChildName, SubStKey, SubAggregators}
                      % SubAggregators = [{files, 0}, {dirs, 0}, {size, 0}] % Free form, add new ones when needed
    extra = [] % Any info important only for this inode
    }).

% Create empty inode with given ID
create(ID) when is_binary(ID) ->
  #inode_v0{id = ID, version = dys_time:now()}.

% Save whole inode in compressed external format
dump(#inode_v0{} = Inode) ->
  erlang:term_to_binary(Inode, [{compressed, 9}, {minor_version, 1}]).

% Generate storage key for given inode.
store_key(#inode_v0{id = ID, version = Version}) when is_binary(ID), is_integer(Version) ->
  iolist_to_binary([ID, $:, integer_to_list(Version)]).

% Restore inode from dump
restore(Bin) ->
  try erlang:binary_to_term(Bin) of
    #inode_v0{} = Inode -> {ok, Inode};
    % Future: Call version converters from here
    _ -> {error, unknown_format}
  catch
    error:badarg ->
      {error, bad_dump}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main live inode API

add_child(Pid, {Name, StorageId, Metadata}) when is_pid(Pid), is_binary(Name), is_binary(StorageId), is_list(Metadata) ->
  gen_server:call(Pid, {add_child, Name, StorageId, Metadata}).

get_child(Pid, Name) when is_pid(Pid), is_binary(Name) ->
  gen_server:call(Pid, {get_child, Name}).

list_children(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, list_children).

describe_children(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, describe_children).

update_child(Pid, {Name, StorageId, Metadata}) when is_pid(Pid), is_binary(Name) ->
  gen_server:call(Pid, {update_child, Name, StorageId, Metadata}).

del_child(Pid, Name) when is_pid(Pid), is_binary(Name) ->
  gen_server:call(Pid, {del_child, Name}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server implementation

-record(state, {
    }).

init([StorageKey]) when is_binary(StorageKey) ->
  {ok, Inode} = restore(?STORAGE:fetch(inode, StorageKey)),
  {ok, {Inode, #state{}}}.

handle_call(_, _, {Inode, State}) ->
  {reply, not_implemented, {Inode, State}}.

handle_info(_, {Inode, State}) ->
  {noreply, {Inode, State}}.

handle_cast(_, {Inode, State}) ->
  {noreply, {Inode, State}}.

terminate(_, _) ->
  ok.

code_change(_, {Inode, State}, _) ->
  {ok, {Inode, State}}.
