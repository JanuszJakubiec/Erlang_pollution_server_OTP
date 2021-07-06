-module(pollution_gen_server).
-author("Janusz").

-export([]).

-behaviour(gen_server).
-export([start/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/3, getLastMeasurement/2, getMeasureMaxValue/2, init/1, stop/0, handle_call/3, handle_cast/2, terminate/2, crash/0]).

start() ->
  gen_server:start_link( {local, monitorServer}, pollution_gen_server, pollution:createMonitor(), []).

init(State) ->
  {ok, State}.

handle_call({modifyState, Function, Args}, _From, State) ->
  {Message, NState} = apply(pollution, Function, Args ++ [State]),
  {reply, Message, NState};
handle_call({getValue, Function, Args}, _From, State) ->
  {reply, apply(pollution, Function, Args ++ [State]), State};
handle_call({stop}, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(crash, State) -> no:exist(), {noreply, State}.

terminate(normal, _) -> io:format("Ended"), ok.

crash() ->
  gen_server:call(monitorServer, crash).

addStation(Name, Coordinates) ->
  gen_server:call(monitorServer, {modifyState, addStation, [Name, Coordinates]}).

addValue(StationIdentifier, MeasurementDate, MeasurementType, MeasurementValue) ->
  gen_server:call(monitorServer, {modifyState, addValue, [StationIdentifier, MeasurementDate, MeasurementType, MeasurementValue]}).

removeValue(StationIdentifier, MeasurementDate, MeasurementType) ->
  gen_server:call(monitorServer, {modifyState, removeValue, [StationIdentifier, MeasurementDate, MeasurementType]}).

getOneValue(StationIdentifier, MeasurementDate, MeasurementType) ->
  gen_server:call(monitorServer, {getValue, getOneValue, [StationIdentifier, MeasurementDate, MeasurementType]}).

getStationMean(StationIdentifier, MeasurementType) ->
  gen_server:call(monitorServer, {getValue, getStationMean, [StationIdentifier, MeasurementType]}).

getDailyMean(Date, Type) ->
  gen_server:call(monitorServer, {getValue, getDailyMean, [Date, Type]}).

getHourlyMean(StationIdentifier, Hour, Type) ->
  gen_server:call(monitorServer, {getValue, getHourlyMean, [StationIdentifier, Hour, Type]}).

getLastMeasurement(StationIdentifier, Type) ->
  gen_server:call(monitorServer, {getValue, getLastMeasurement, [StationIdentifier, Type]}).

getMeasureMaxValue(StationIdentifier, Type) ->
  gen_server:call(monitorServer, {getValue, getMeasureMaxValue, [StationIdentifier, Type]}).

stop() ->
  gen_server:call(monitorServer, {stop}).
