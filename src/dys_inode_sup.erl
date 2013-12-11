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
  {ok, {{one_for_one, 1000, 1}, []}}.

inode_spec(Name, Args) ->
  {Name,
   {dys_inode_srv, start_link, Args},
   temporary, 1000, worker, [dys_inode_srv, dys_inode]}.

% Request to serve given inode
serve(Inode) ->
  serve(whereis(?MODULE), Inode).

serve(Sup, Inode) ->
  Name = dys_inode:id(Inode),
  supervisor:start_child(Sup, inode_spec(Name, [serve, Inode, Sup])).


% Request to read inode from DB by key and serve it
revive(StoreKey) ->
  revive(whereis(?MODULE), StoreKey).

revive(Sup, StoreKey) ->
  Name = dys_inode:id(StoreKey),
  case supervisor:start_child(Sup, inode_spec(Name, [revive, StoreKey, Sup])) of
    {ok, Pid} ->
      {ok, Pid};
    {error, {already_started, Pid}} ->
      {ok, Pid};
    Other ->
      Other
  end.
