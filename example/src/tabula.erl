-module (tabula).

-behaviour (application).
-behaviour (supervisor).

-export ([demo/0, empty/0, proplist/0]).
-export([start/0, stop/0, start/2, stop/1, init/1]).

empty() ->
  io:format("Empty list~n"),
  list_as_table:print([]).

proplist() ->
  List = [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}],
  io:format("Proplist as table:~n"),
  list_as_table:print(List)m.

demo() ->
  Random = random_proplist:make(7, 5),
  io:format("Random 7 elements proplist:~n~p~n~n", [Random]),
  io:format("5x7 table from the proplist:~n"),
  list_as_table:print(Random),
  io:format("The same on standard_error:~n"),
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
