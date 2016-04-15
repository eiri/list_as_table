-module (tabula).

-behaviour (application).
-behaviour (supervisor).

-export ([demo/0]).
-export([start/0, stop/0, start/2, stop/1, init/1]).

demo() ->
  Random = random_proplist:make(7, 5),
  io:format("Random 7 elements proplist:~n~p~n~n", [Random]),
  io:format("5x7 table from the proplist:~n"),
  list_as_table:print(Random),
  io:format("The same on standard_error~n"),
  list_as_table:print(standard_error, Random).

%% callbacks

start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  ok = application:stop(?MODULE).

start(_Type, _StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

init([]) ->
  {ok, {{one_for_one, 3, 3600}, []}}.
