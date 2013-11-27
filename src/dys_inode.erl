-module(dys_inode).
-include("dys_inode.hrl").

-behavior(gen_server).

% low-level inode functions. Used for debug, no real need in working system
-export([create/1, dump/1, store_key/1, restore/1]).

% inode server API
-export([start_link/1]).
-export([add_child/3, get_child/2, list_children/1, describe_children/1, update_child/2, del_child/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Fire up supervisor
start_link(StoreKey) ->
  gen_server:start_link(?MODULE, [self(), StoreKey], []).


% Min and max child count. When there are few children, aggregation or local rebalancing fires up.
-define(MIN_CHILDREN, 5).
-define(MAX_CHILDREN, 25).
-define(BAL_CHILDREN, 15). % balanced child count

% Storage adapter
-define(STORAGE, dys_storage_mongo).


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

% Insert child replacing possibly existing one
insert_inode_child(Spec, #inode_v0{children = Children} = Inode) ->
  NewChildren = lists:ukeymerge(1, [Spec], Children),
  Inode#inode_v0{version = dys_time:now(), children = NewChildren}.

% Make summary for this inode
make_summary(#inode_v0{root = true} = Inode) ->
  % We are root, so our summary is directory child in leaf node
  {undefined, dir, store_key(Inode), gather_metadata(Inode)};

make_summary(#inode_v0{root = false, children = Children} = Inode) ->
  % We are root, so our summary is directory child in leaf node
  Metadata = gather_metadata(Inode),
  {Min, _, _, _} = hd(Children),
  {Max, _, _, _} = lists:last(Children),
  {{Min, Max}, {inode, proplists:get_value(records, Metadata)}, store_key(Inode), Metadata}.

gather_metadata(#inode_v0{leaf = true, children = Children}) ->
  % Leaf metadata is currently number of records=children
  [{records, length(Children)}];
gather_metadata(#inode_v0{leaf = false, children = Children}) ->
  RecordCount = lists:sum([RC || {_, {inode, RC}, _, _} <- Children]),
  [{records, RecordCount}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main live inode API

% TODO: [<<"path">>, <<"to">> | {<<"Child">>, file, <<"xxyy">>, [{size, 100500}]}]
add_child(Pid, ChildSpec, Options) ->
  gen_server:call(Pid, {add_child, ChildSpec, Options}).

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
    inode = undefined,
    owners = [],
    pid_cache = gb_trees:empty()
    }).

init([OwnerPid, StorageKey]) when is_pid(OwnerPid), is_binary(StorageKey) ->
  {ok, Inode} = restore(?STORAGE:fetch(inode, StorageKey)),
  {ok, #state{inode = Inode, owners = [OwnerPid]}}.

handle_call({add_child, ChildSpec, Options}, _, #state{} = State) ->
  case do_add_child(ChildSpec, Options, State) of
    {ok, #state{inode = Inode} = NewState} ->
      {reply, {ok, make_summary(Inode)}, NewState};
    {error, Error, NewState} -> 
      {reply, {error, Error}, NewState}
  end;

handle_call(_, _, #state{} = State) ->
  {reply, {error, not_implemented}, State}.

handle_info(_, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

code_change(_, #state{} = State, _) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internals

valid_childspec({Name, Type, StoreKey, Metadata}) when is_binary(Name), is_binary(StoreKey), is_list(Metadata) ->
  valid_childtype(Type);
valid_childspec(_) ->
  false.

valid_childtype(file) -> true;
valid_childtype(dir) -> true;
valid_childtype(_) -> false.


do_add_child(ChildSpec, Options, State) ->
  case valid_childspec(ChildSpec) of
    true -> do_add_valid_child(ChildSpec, Options, State);
    false -> {error, invalid_childspec, State}
  end.

do_add_valid_child({_Name, _, _, _} = Spec, _Options, #state{inode = Inode} = State) when ?LEAF(Inode) ->
  NewInode = insert_inode_child(Spec, Inode),
  {ok, State#state{inode = NewInode}}.

