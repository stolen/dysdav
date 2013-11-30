-module(dys_inode_srv).
-include("dys_inode.hrl").

-behavior(gen_server).

% Storage adapter
-define(STORAGE, dys_storage_mongo).

% inode server API
-export([start_link/1]).
-export([add_child/3, get_child/2, list_children/1, describe_children/1, update_child/2, del_child/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Fire up supervisor
start_link(StoreKey) ->
  gen_server:start_link(?MODULE, [self(), StoreKey], []).

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
  {ok, Inode} = dys_inode:restore(?STORAGE:fetch(inode, StorageKey)),
  {ok, #state{inode = Inode, owners = [OwnerPid]}}.

handle_call({add_child, ChildSpec, Options}, _, #state{} = State) ->
  case do_add_child(ChildSpec, Options, State) of
    {ok, #state{inode = Inode} = NewState} ->
      {reply, {ok, dys_inode:make_summary(Inode)}, NewState};
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
  NewInode = dys_inode:insert_inode_child(Spec, Inode),
  {ok, State#state{inode = NewInode}}.

