-define(INODE_ORDER, 100).

-define(MAX_CHILDREN,
%        ?INODE_ORDER).
        3). % test

-define(MIN_CHILDREN,
%        ?INODE_ORDER div 3). % Choose good value to avoid often merges but keep inodes pretty full
        1). % test

% Early version: directory tree is B-tree with metadata cache
-record(inode_v0, {
    root = true, % This means that inode is directory entry and must update directory latest StoreKey
    leaf = true, % This means children are records, (other inodes otherwise)
    % id is persistent, every inode has its own id
    id = undefined, % There should be map of ID -> LatestVersion for root inodes
    version = 0, % Incrementing version number
    parent = undefined, % parent {Id, Version}
    actions = [], % Actions between previous and current version. [{Timestamp, Action}]
    children = [], % sorted list of children (see below)
    extra = [] % Any info important only for this inode
    }).

% resource child (record) structure (leaf = true):
% {Name, Type, StoreKey, Metadata}
%   | Type :: file|dir|shared
%   | Metadata :: [{Key, Value}]
%
% inode child structure (leaf = false):
% {{Min, Max}, {inode, RecordCount}, StoreKey, Metadata}


-define(LEAF(Inode), Inode#inode_v0.leaf).
