%% @doc Simple random proplist generator
%% @version 0.1
%% @reference <a href="https://github.com/eiri/list_as_table">https://github.com/eiri/list_as_table</a>
%% @author Eric Avdey <eiri@eiri.ca>
%% @copyright 2014 Eric Avdey

-module(random_proplist).

-export ([make/0, make/1, make/2]).

-type proplist() :: [tuple()].

%% @doc Generates a list with 12 proplists, 5 tuples in each.
-spec make() -> [proplist()].
make() ->
  make(5, 12).

%% @doc Generates a list with Rows number of proplists, 5 tuples in each.
-spec make(Rows::integer()) -> [proplist()].
make(Rows) ->
  make(5, Rows).

%% @doc Generates a list with Rows number of roplists, Cols tuples in each.
-spec make(Rows::integer(), Cols::integer()) -> [proplist()].
make(Rows, Cols) ->
  HeadPattern = [word || _ <- lists:seq(1,Cols)],
  LinePattern = [rand_type() || _ <- lists:seq(1,Cols)],
  Headers = rand_line(HeadPattern),
  [lists:zip(Headers, rand_line(LinePattern)) ||  _ <- lists:seq(1, Rows)].

%% private

rand_line(Pattern) ->
  [rand_thing(P) || P <- Pattern].

rand_thing(word) -> rand_word(crypto:rand_uniform(3, 13));
rand_thing(int) -> rand_int(42);
rand_thing(float) -> rand_float(64, 3).

rand_type() ->
  case crypto:rand_uniform(0, 3) of
    0 -> word;
    1 -> int;
    2 -> float
  end.

rand_int(N) ->
  crypto:rand_uniform(0, N+1).

rand_float(N, Pr) ->
  F = rand_int(N) * (crypto:rand_uniform(0, 30323) / 30323.0),
  P = math:pow(10, Pr),
  round(F * P) / P.

rand_char() ->
  crypto:rand_uniform(1, 27) + 96.

rand_word(N) -> rand_word(N, []).
rand_word(0, Acc) -> Acc;
rand_word(N, Acc) -> rand_word(N - 1, [rand_char()|Acc]).