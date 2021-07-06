-module(pollution_tests).
-author("Janusz").
-include_lib("eunit/include/eunit.hrl").

-export([]).

createMonitorWithMultipleStations(_,Tab,0) -> createMonitorWithMultipleStations(pollution:createMonitor(), Tab, 1);
createMonitorWithMultipleStations(Monitor, [], 1) -> Monitor;
createMonitorWithMultipleStations(Monitor, [Head|Tail], 1) ->
  {Name, Cords} = Head,
  createMonitorWithMultipleStations(unpack(pollution:addStation(Name, Cords, Monitor)), Tail, 1).

createMultipleMeasurements(Monitor, []) -> Monitor;
createMultipleMeasurements(Monitor, [Head|Tail]) ->
  {Key, Date, Type, Value} = Head,
  createMultipleMeasurements(unpack(pollution:addValue(Key, Date, Type, Value, Monitor)), Tail).

unpack({_, Monitor}) ->
  Monitor.

deleteMultipleMeasurements(Monitor, []) -> Monitor;
deleteMultipleMeasurements(Monitor, [Head|Tail]) ->
  {Key, Date, Type} = Head,
  deleteMultipleMeasurements(unpack(pollution:removeValue(Key, Date, Type, Monitor)), Tail).

adding_station_test() ->
  {ok, [{"AA", {1,2}, []}]} = pollution:addStation("AA", {1,2}, pollution:createMonitor()).

adding_existing_station_test() ->
  {stationExists,[{"AA", {1,2}, []}]} = pollution:addStation("AA", {1,2}, unpack(pollution:addStation("AA", {1,2},pollution:createMonitor()))).

adding_multiple_stations_test() ->
  [{"AAB", {1,3}, []},{"AA", {1,2}, []}] = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0).

adding_measurement_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB",{1,3},[]},{"AA",{1,2},[{{{2000, 1, 1},{1, 1, 1}},"PM-10",100}]}] = createMultipleMeasurements(Tab, [{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

adding_measurements_to_clear_table_test() ->
  Tab = createMonitorWithMultipleStations(null, [], 0),
  {noSuchStation,[]} = pollution:addValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100, Tab).

adding_the_same_measurements_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  {measurementExists,[{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}]} = pollution:addValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100, unpack(pollution:addValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100, Tab))).

adding_measurement_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

adding_the_same_measurements_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  {measurementExists, [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}]} = pollution:addValue("AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100, unpack(pollution:addValue("AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100, Tab))).

adding_multiple_measurements_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 2, 1},{1, 1, 1}}, "PM-10", 100}, {{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = createMultipleMeasurements(Tab, [{"AA", {{2000, 2, 1},{1, 1, 1}}, "PM-10", 100},{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

adding_multiple_measurements_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 2, 1},{1, 1, 1}}, "PM-10", 100}, {{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = createMultipleMeasurements(Tab, [{{1,2}, {{2000, 2, 1},{1, 1, 1}}, "PM-10", 100},{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

adding_multiple_measurements2_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB", {1,3}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AAB", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

adding_multiple_measurements2_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  [{"AAB", {1,3}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = createMultipleMeasurements(Tab, [{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{{1,3}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]).

deleting_measurement_from_clear_monitor_test() ->
  Tab = createMonitorWithMultipleStations(null, [],  0),
  {noSuchStation, []} = pollution:removeValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", Tab).

deleting_measurement_from_clear_table_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  {noSuchMeasurement, [{"AAB", {1,3}, []},{"AA", {1,2}, []}]} = pollution:removeValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", Tab).

deleting_measurement_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  {ok, [{"AAB", {1,3}, []},{"AA", {1,2}, []}]} = pollution:removeValue({1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10",Tab1).

deleting_measurement_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  {ok, [{"AAB", {1,3}, []},{"AA", {1,2}, []}]} = pollution:removeValue("AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10",Tab1).

deleting_multiple_measurements_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AA", {{2000, 2, 1},{1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, []},{"AA", {1,2}, []}] = deleteMultipleMeasurements(Tab1, [{"AA", {{2000, 2, 1},{1, 1}}, "PM-10"},{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10"}]).

deleting_multiple_measurements_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 2, 1},{1, 1, 1}}, "PM-10", 100},{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, []},{"AA", {1,2}, []}] = deleteMultipleMeasurements(Tab1, [{{1,2}, {{2000, 1, 1},{1, 1, 1}}, "PM-10"},{{1,2}, {{2000, 2, 1},{1, 1, 1}}, "PM-10"}]).

deleting_multiple_measurements2_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AAB", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = deleteMultipleMeasurements(Tab1,[{"AAB", {{2000, 1, 1},{1, 1, 1}}, "PM-10"}]).

deleting_multiple_measurements2_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AAB", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, []},{"AA", {1,2}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]}] = deleteMultipleMeasurements(Tab1,[{{1,3}, {{2000, 1, 1},{1, 1, 1}}, "PM-10"}]).

deleting_multiple_measurements3_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AAB", {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, [{{{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]},{"AA", {1,2}, []}] = deleteMultipleMeasurements(Tab1,[{"AA", {{2000, 1, 1},{1, 1, 1}}, "PM-10"}]).

deleting_multiple_measurements3_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AAB",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  [{"AAB", {1,3}, [{ {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]},{"AA", {1,2}, []}] = deleteMultipleMeasurements(Tab1,[{{1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-10"}]).

getting_measurement_by_cord_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{{1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{{1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100}]),
  null = pollution:getOneValue({1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-2.5", Tab1).

getting_measurement_by_name_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 100},{"AA",  {{2000, 1, 11},{1, 1, 1}}, "PM-10", 100}]),
  null = pollution:getOneValue("AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-2.5", Tab1).

getting_measurement_by_cord_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{{1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 50},{{1,2},  {{2000, 11, 1},{1, 1, 1}}, "PM-10", 100}]),
  50 = pollution:getOneValue({1,2},  {{2000, 1, 1},{1, 1, 1}}, "PM-10", Tab1).

getting_measurement_by_name_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 50},{"AA",  {{2000, 1, 11},{1, 1, 1}}, "PM-10", 100}]),
  50 = pollution:getOneValue("AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", Tab1).

getting_station_mean_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-10", 50},{"AA",  {{2000, 11, 1},{1, 1, 1}}, "PM-10", 100}]),
  75.0 = pollution:getStationMean("AA","PM-10", Tab1).

getting_station_mean_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA",  {{2000, 1, 1},{1, 1, 1}}, "PM-2.5", 50},{"AA",  {{2000, 11, 1},{1, 1, 1}}, "PM-2.5", 100}]),
  null = pollution:getStationMean("AA","PM-10", Tab1).

getting_daily_mean_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1, 1},{1,1,1}}, "PM-2.5", 50},{"AA", {{2000,1, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{10,2,1}}, "PM-2.5", 100}]),
  87.5 = pollution:getDailyMean({2000,1, 1}, "PM-2.5", Tab1).

getting_daily_mean_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1, 1},{1,1,1}}, "PM-2.5", 50},{"AA", {{2000,1, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{10,2,1}}, "PM-2.5", 100}]),
  null = pollution:getDailyMean({2000,1, 1}, "PM-15", Tab1).

getting_daily_mean_3_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1, 1},{1,1,1}}, "PM-2.5", 50},{"AA", {{2000, 2, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{1,2,1}}, "PM-2.5", 100},{"AAB", {{2000,1, 1},{10,2,1}}, "PM-2.5", 100}]),
  (250/3) = pollution:getDailyMean({2000,1, 1}, "PM-2.5", Tab1).

getting_hourly_mean_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 12, 1},{11,1,1}}, "PM-2.5", 50},{"AA", {{2000,1, 1},{1,1,11}}, "PM-2.5", 100},{"AAB", {{2000, 11, 1},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000, 10, 1},{1,1,1}}, "PM-2.5", 100}]),
  null = pollution:getHourlyMean("AA",{1,1,1}, "PM-2.5", Tab1).

getting_hourly_mean_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 12, 1},{1,1,11}}, "PM-2.5", 50},{"AA", {{2000, 3, 1},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000, 11, 1},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000, 10, 1},{1,1,1}}, "PM-2.5", 100}]),
  100.0 = pollution:getHourlyMean("AA",{1,1,1}, "PM-2.5", Tab1).

getting_hourly_mean_3_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 12, 1},{1,11,1}}, "PM-2.5", 50},{"AA", {{2000, 2, 1},{1,1,1}}, "PM-2.5", 10},{"AA", {{2000, 11, 1},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,12},{1,1,1}}, "PM-2.5", 100}]),
  55.0 = pollution:getHourlyMean("AA",{1,1,1}, "PM-2.5", Tab1).

getting_maximal_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000, 12, 1},{11,1,1}}, "PM-2.5", 50},{"AA", {{2000,1, 1},{1,1,11}}, "PM-2.5", 100},{"AAB", {{2000, 11, 1},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,12},{1,1,1}}, "PM-2.5", 100}]),
  null = pollution:getMeasureMaxValue("AA", "PM-10", Tab1).

getting_maximal_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1,12},{1,1,11}}, "PM-2.5", 50},{"AA", {{2000,11,12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,12},{1,1,1}}, "PM-2.5", 100}]),
  100 = pollution:getMeasureMaxValue("AA", "PM-2.5", Tab1).


getting_maximal_3_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1,12},{1,11,1}}, "PM-2.5", 50},{"AA", {{2000,1,12},{1,1,1}}, "PM-2.5", 10},{"AA", {{2000,1,12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,11,12},{1,1,1}}, "PM-2.5", 100}]),
  null = pollution:getMeasureMaxValue("AABS", "PM-10", Tab1).

getting_last_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1,12},{1,11,1}}, "PM-2.5", 50},{"AA", {{2000,2,15},{1,1,1}}, "PM-2.5", 10},{"AA", {{2000, 2, 12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,20},{1,1,1}}, "PM-2.5", 100}]),
  {{{2000,1,20},{1,1,1}}, "PM-2.5", 100} = pollution:getLastMeasurement("AAB", "PM-2.5", Tab1).

getting_last_2_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1,12},{1,11,1}}, "PM-2.5", 50},{"AA", {{2000,2,15},{1,1,1}}, "PM-2.5", 10},{"AA", {{2000, 2, 12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,20},{1,1,1}}, "PM-2.5", 100}]),
  null = pollution:getLastMeasurement("AAB", "PM-10", Tab1).

getting_last_3_test() ->
  Tab = createMonitorWithMultipleStations(null, [{"AA", {1,2}}, {"AAB", {1,3}}], 0),
  Tab1 = createMultipleMeasurements(Tab, [{"AA", {{2000,1,12},{1,11,1}}, "PM-2.5", 50},{"AA", {{2000,2,15},{1,1,1}}, "PM-2.5", 10},{"AA", {{2000, 2, 12},{1,1,1}}, "PM-2.5", 100},{"AAB", {{2000,1,20},{1,1,1}}, "PM-2.5", 100}]),
  {{{2000,2,15},{1,1,1}},"PM-2.5",10} = pollution:getLastMeasurement("AA", "PM-2.5", Tab1).