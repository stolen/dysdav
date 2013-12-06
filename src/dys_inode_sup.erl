-module(dys_inode_sup).
-behavior(supervisor).

% Supervisor API
-export([start_link/0]).
-export([init/1]).

% Interface
-export([serve/1, revive/1]).
-export([serve/2, revive/2]).



% Supervisor stuff
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  InodeSpec = {undefined,
               {dys_inode_srv, start_link, []},
               temporary, 1000, worker, [dys_inode_srv, dys_inode]},
  {ok, {{simple_one_for_one, 1000, 1}, [InodeSpec]}}.


% Request to serve given inode
serve(Inode) ->
  serve(whereis(?MODULE), Inode).

serve(Sup, Inode) ->
  supervisor:start_child(Sup, [serve, Inode, Sup]).


% Request to read inode from DB by key and serve it
revive(StoreKey) ->
  revive(whereis(?MODULE), StoreKey).

revive(Sup, StoreKey) ->
  supervisor:start_child(Sup, [revive, StoreKey, Sup]).
