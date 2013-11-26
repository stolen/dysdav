-module(dys_time).
-export([now/0]).
-compile({no_auto_import, [{now, 0}]}).

now() ->
  {Mega, Sec, Micro} = os:timestamp(),
  Mega*1000000000 + Sec*1000 + Micro div 1000.
