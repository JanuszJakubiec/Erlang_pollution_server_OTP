-module(pollution_value_collector_gen_statem).
-author("Janusz").
-behaviour(gen_statem).

%% API
-export([callback_mode/0, init/1, start_link/0, store_data/0, add_value/3, set_station/2, stop/0, values_insertion/3, waiting_for_station/3, terminate/3]).

set_station(Name, Coordinates) ->
  gen_statem:call(add_multiple_measures, {station, Name, Coordinates}).

add_value(Date, Type, Value) ->
  gen_statem:call(add_multiple_measures, {value, Date, Type, Value}).

store_data() ->
  gen_statem:call(add_multiple_measures, store).

start_link() ->
  gen_statem:start_link({local, add_multiple_measures}, pollution_value_collector_gen_statem, [], []).

init([]) -> {ok, waiting_for_station, []}.

callback_mode() -> state_functions.
stop() -> gen_statem:stop(add_multiple_measures).

terminate(_, _, _) -> ok.

waiting_for_station({call, From}, {station, Name, Coordinates}, _) ->
  {next_state, values_insertion, {Name, Coordinates, []}, [{reply, From, values_insertion}]}.
values_insertion({call, From}, {value, Date, Type, Value}, {Name, Coordinates, Table}) ->
  {keep_state, {Name, Coordinates, Table ++ [{Date, Type, Value}]}, [{reply, From, values_insertion}]};
values_insertion({call, From}, store, Data) ->
  {next_state, waiting_for_station, [], [{reply, From, add_values_to_the_server(Data)}]}.

add_values_to_the_server({Name, Coordinates, Table}) ->
  pollution_gen_server:addStation(Name, Coordinates),
  add_values_to_the_server({Name, ok, Table, stationPresent});
add_values_to_the_server({_, ok, [], stationPresent}) ->
  ok;
add_values_to_the_server({Name, ok,  [{Date, Type, Value} | Tail], stationPresent}) ->
  add_values_to_the_server({Name, pollution_gen_server:addValue(Name, Date, Type, Value), Tail, stationPresent});
add_values_to_the_server({_, Message, _, stationPresent}) ->
  Message.
