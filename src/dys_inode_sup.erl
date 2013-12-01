-module(dys_inode_sup).
-behavior(supervisor).

% Supervisor API
-export([start_link/0]).
-export([init/1]).

% Interface
-export([serve/1, revive/1]).



% Supervisor stuff
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  InodeSpec = {undefined,
               {dys_inode_srv, start_link, []},
               temporary, 1000, worker, [dys_inode_srv, dys_inode]},
  {ok, {{simple_one_for_one, 1000, 1}, [InodeSpec]}}.


% Request to serve already existing inode
serve(Inode) ->
  supervisor:start_child(?MODULE, [serve, Inode]).

% Request to read inode from DB by key and serve it
revive(StoreKey) ->
  supervisor:start_child(?MODULE, [revive, StoreKey]).
