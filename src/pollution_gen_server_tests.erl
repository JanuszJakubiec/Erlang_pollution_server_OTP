-module(pollution_gen_server_tests).
-author("Janusz").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

pollution_gen_server_start_test() ->
  {{ok, _}, ok} = {pollution_gen_server:start(), pollution_gen_server:stop()}.

pollution_gen_server_adds_new_station_test() ->
  pollution_gen_server:start(),
  {ok, ok} = {pollution_gen_server:addStation("aaa", {1,3}), pollution_gen_server:stop()}.

pollution_gen_server_adds_measurement_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  {ok, {{{2000, 10, 10},{10, 10, 10}}, "PM-10", 100 }, ok} = { pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100), pollution_gen_server:getLastMeasurement("aaa", "PM-10"), pollution_gen_server:stop()}.

pollution_gen_server_deletes_measurement_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  {ok, null,  ok} = {pollution_gen_server:removeValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10"), pollution_gen_server:getLastMeasurement("aaa", "PM-10") , pollution_gen_server:stop()}.

pollution_gen_server_get_one_value_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  {100, ok} = {pollution_gen_server:getOneValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10"), pollution_gen_server:stop()}.

pollution_gen_server_get_station_mean_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 12, 10}}, "PM-10", 50),
  {75.0, ok} = {pollution_gen_server:getStationMean("aaa", "PM-10"), pollution_gen_server:stop()}.

pollution_gen_server_get_daily_mean_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 12, 10}}, "PM-10", 50),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{9, 12, 10}}, "PM-10", 30),
  pollution_gen_server:addValue("aaa", {{2000, 11, 10},{9, 12, 10}}, "PM-10", 30),
  {60.0, ok} = {pollution_gen_server:getDailyMean({2000, 10,10}, "PM-10"), pollution_gen_server:stop()}.

pollution_gen_server_get_hourly_mean_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  pollution_gen_server:addValue("aaa", {{2000, 11, 10},{10, 10, 10}}, "PM-10", 50),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{9, 12, 10}}, "PM-10", 30),
  pollution_gen_server:addValue("aaa", {{2000, 11, 10},{9, 12, 10}}, "PM-10", 30),
  {75.0, ok} = {pollution_gen_server:getHourlyMean("aaa", {10,10,10}, "PM-10"), pollution_gen_server:stop()}.

pollution_gen_server_get_measure_max_value_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("aaa", {1,3}),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{10, 10, 10}}, "PM-10", 100),
  pollution_gen_server:addValue("aaa", {{2000, 10, 10},{9, 12, 10}}, "PM-10", 30),
  pollution_gen_server:addValue("aaa", {{2000, 11, 10},{9, 12, 10}}, "PM-10", 230),
  {230, ok} = {pollution_gen_server:getMeasureMaxValue("aaa", "PM-10"), pollution_gen_server:stop()}.
