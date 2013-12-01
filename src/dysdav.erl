-module(dysdav).
-behavior(application).
-behavior(supervisor).

% Simple API
-export([start/0, stop/0]).

% Application exports
-export([start/2, stop/1]).

% Supervisor exports
-export([init/1]).

start() ->
  application:start(dysdav).

start(_, _) ->
  dys_storage_mongo:setup(),
  supervisor:start_link({local, dysdav}, ?MODULE, []).


stop() ->
  application:stop(dysdav).

stop(_) ->
  ok.


init([]) ->
  INodeSupSpec = {inodes,
                  {dys_inode_sup, start_link, []},
                  permanent, 10000, supervisor, [dys_inode_sup]},

  {ok, {{one_for_one, 10, 1}, [INodeSupSpec]}}.
