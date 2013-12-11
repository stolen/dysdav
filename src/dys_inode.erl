-module(dys_inode).
-include("dys_inode.hrl").

% low-level inode functions.
-export([create/0, dump/1, id/1, store_key/1, restore/1]).

% High-level inode interface
-export([check/1, make_summary/1]).
-export([insert_inode_child/2, needs_split/1]).
-export([where_insert/2, finish_insert/3]).


% Check inode, future: possibly fix
check(#inode_v0{} = Inode) -> Inode.

create() ->
  create(gen_id()).

% Create empty inode with given ID
create(ID) when is_binary(ID) ->
  #inode_v0{id = ID}.

% Generate unique traceable inode id
gen_id() ->
  NodeHash = erlang:adler32(atom_to_binary(node(), latin1)),
  UniqueTime = dys_time:unique(),
  iolist_to_binary([
      erlang:integer_to_list(NodeHash, 36),
      ".",
      erlang:integer_to_list(UniqueTime, 36) ]).

% Save whole inode in compressed external format
dump(#inode_v0{} = Inode) ->
  erlang:term_to_binary(Inode, [{compressed, 9}, {minor_version, 1}]).

% Generate storage key for given inode.
store_key(#inode_v0{id = ID, version = Version}) when is_binary(ID), is_integer(Version) ->
  iolist_to_binary([ID, $:, integer_to_list(Version)]).

id(#inode_v0{id = ID}) ->
  ID;
id(StoreKey) when is_binary(StoreKey) ->
  [ID, _] = binary:split(StoreKey, <<":">>),
  ID.

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
  {minmax(Min, Max), {inode, proplists:get_value(records, Metadata)}, store_key(Inode), Metadata}.

gather_metadata(#inode_v0{leaf = true, children = Children}) ->
  % Leaf metadata is currently number of records=children
  [{records, length(Children)}];
gather_metadata(#inode_v0{leaf = false, children = Children}) ->
  RecordCount = lists:sum([RC || {_, {inode, RC}, _, _} <- Children]),
  [{records, RecordCount}].

minmax({Min, _}, {_, Max}) -> {Min, Max};
minmax(Min, Max) -> {Min, Max}.


needs_split(#inode_v0{children = Children} = Inode) ->
  needs_split(Inode, length(Children)).

needs_split(_Inode, Len) when Len =< ?MAX_CHILDREN ->
  false;
needs_split(#inode_v0{root = false, leaf = Leaf, id = Id, version = Ver0,
                      children = Children} = Inode0, Len) ->
  {Ch1, Ch2} = lists:split(Len div 2, Children),
  Now = dys_time:now(),
  % Inode1 is just evolution of local inode
  Inode1 = Inode0#inode_v0{version = Ver0 + 1,
                           parent = {Id, Ver0}, actions = [{Now, split}],
                           children = Ch1},
  % Inode2 is newly created sibling
  Inode2 = #inode_v0{root = false, leaf = Leaf, id = gen_id(),
                     parent = {Id, Ver0}, actions = [{Now, split}],
                     children = Ch2},
  {sibling, Inode1, Inode2};

needs_split(#inode_v0{root = true, leaf = Leaf, id = Id, version = Ver0,
                      children = Children} = Inode0, Len) ->
  {Ch1, Ch2} = lists:split(Len div 2, Children),

  Now = dys_time:now(),
  ChildSkel = #inode_v0{root = false, leaf = Leaf, parent = {Id, Ver0},
                        actions = [{Now, split_root}]},

  Inode1 = ChildSkel#inode_v0{id = gen_id(), children = Ch1},
  Inode2 = ChildSkel#inode_v0{id = gen_id(), children = Ch2},
  NewRoot = Inode0#inode_v0{leaf = false, version = Ver0 + 1,
                            parent = {Id, Ver0}, actions = [{Now, new_level}],
                            children = [
        make_summary(Inode1),
        make_summary(Inode2) ]},

  {children, NewRoot, [Inode1, Inode2]}.


where_insert(Name, #inode_v0{children = Children}) ->
  take_insert_location(Name, [], Children).

% Some kind of zipper here: search for good place for key insertion and return matching child and other children as zipper
take_insert_location(Name, Less, [{{_Min, Max}, {inode, _}, _, _} = Cur|Greater]) when Max < Name ->
  % This inode has too low key range, continue search
  take_insert_location(Name, [Cur|Less], Greater);
take_insert_location(Name, Less, [{{_Min, Max}, {inode, _}, _, _} = Cur|Greater]) when Name =< Max ->
  % Found child inode with appropriate key range
  % possibly key is too small for next children, but pass it there (TODO: possible balancing here)
  {Cur, {Less, Greater}};
take_insert_location(_Name, [Last|Less], []) ->
  % All child inodes contain keys that compare less to Name -> select last (greatest) one
  {Last, {Less, []}}.
 
% Construct back child list
finish_insert(InsertResult, {Less, Greater}, #inode_v0{} = Inode) ->
  NewChildren = lists:reverse(Less, InsertResult ++ Greater),
  Inode#inode_v0{children = NewChildren}.
