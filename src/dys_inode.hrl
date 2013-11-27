-define(INODE_ORDER, 100).

% Early version: directory tree is B-tree with metadata cache
-record(inode_v0, {
    root = true, % This means that inode is directory entry and must update directory latest StoreKey
    leaf = true, % This means children are records, (other inodes otherwise)
    % id is persistent, every inode has its own id
    id = undefined, % There should be map of ID -> LatestStoreKey for root inodes
    version = 0, % put millisecond timestamp here
    prev_version = undefined, % pointer to previous version if any
    last_action = undefined, % when there is previous version, it is good to keep change that lead to this one
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
