-module(dys_inode).
-include("dys_inode.hrl").

% low-level inode functions.
-export([create/1, dump/1, store_key/1, restore/1]).

% High-level inode interface
-export([make_summary/1, insert_inode_child/2]).


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

