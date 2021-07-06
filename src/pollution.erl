-module(pollution).
-include_lib("eunit/include/eunit.hrl").
-author("Janusz Jakubiec").

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/4, getMeasureMaxValue/3,getLastMeasurement/3]).

createMonitor() -> [].

addStation(Name, Coordinates, Table) ->
  A = doesTableContainStation(Table, Name, Coordinates),
  if
    A -> {ok, [{Name, Coordinates, [] } | Table]};
    true -> {stationExists, Table}
  end.

addValue(Identifier, Date, Type, Value, Table) ->
  Add = fun(Beg, Date, Type, Value, Station, Tail) -> addMeasurement(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Add, Date, Type, Value, Table, []).

removeValue(Identifier, Date, Type, Table) ->
  Del = fun(Beg, Date, Type, Value, Station, Tail) -> delMeasurement(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Del, Date, Type, 0, Table, []).

getOneValue(Identifier, Date, Type, Table) ->
  Get = fun(Beg, Date, Type, Value, Station, Tail) -> getMeasurement(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Get, Date, Type, 0, Table, []).

getStationMean(Identifier, Type, Table) ->
  Mean = fun(Beg, Date, Type, Value, Station, Tail) -> meanMeasurements(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Mean, 0, Type, 0, Table, []).

getDailyMean(Date, Type, Table) -> sumAndCountMeasurementsFromDate(Date,Type,Table, 0, 0).

getHourlyMean(Identifier, Hour, Type, Table) ->
  HourMean = fun(Beg, Date, Type, Value, Station, Tail) -> getStationsHourlyMean(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, HourMean, Hour, Type, 0, Table, []).

getStationsHourlyMean(_,_,_,_,{},_) -> null;
getStationsHourlyMean(_,Hour, Type, _, {_, _, Table}, _) ->
  {I, J} = hourlyMeanHelper(Table, Hour, Type, 0, 0),
  if
    J == 0 -> null;
    true -> I/J
  end.

getMeasureMaxValue(Identifier, Type, Table) ->
  Max = fun(Beg, Date, Type, Value, Station, Tail) -> getMaximal(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Max, 0, Type, 0, Table, []).

getLastMeasurement(Identifier, Type, Table) ->
  Last = fun(Beg, Date, Type, Value, Station, Tail) -> lastMeasurement(Beg, Date, Type, Value, Station, Tail) end,
  getStation(Identifier, Last, 0, Type, 0, Table, []).

lastMeasurement(_,_,_,_,{},_) -> null;
lastMeasurement(_,_,Type,_,Station,_) ->
  {_,_,Table} = Station,
  getLastHelper(Table, Type, {}).

getLastHelper([],_, {}) -> null;
getLastHelper([],_, Result) -> Result;
getLastHelper([{Date,Type,Measurement}|Tail], Type, {}) ->
  getLastHelper(Tail, Type, {Date, Type, Measurement});
getLastHelper([_|Tail], Type, {}) ->
  getLastHelper(Tail, Type, {});
getLastHelper([{HDate, Type,Measurement}|Tail], Type, {RDate,Type,RMeasurement}) ->
  Rsec = calendar:datetime_to_gregorian_seconds(RDate),
  Hsec = calendar:datetime_to_gregorian_seconds(HDate),
  if
    (Rsec < Hsec) -> getLastHelper(Tail, Type,{HDate, Type, Measurement});
    true -> getLastHelper(Tail, Type, {RDate, Type, RMeasurement})
  end;
getLastHelper([_|Tail], Type, Result) ->
    getLastHelper(Tail, Type, Result).

getMaximal(_,_,_,_,{},_) -> null;
getMaximal(_,_,Type,_, Station,_) ->
  {_,_,Table} = Station,
  getMaximalHelper(Table, Type, null).

getMaximalHelper([],_, Current) ->
  Current;
getMaximalHelper([{_, Type, Value}|Tail],Type, null) ->
  getMaximalHelper(Tail, Type, Value);
getMaximalHelper([{_, Type, Value}|Tail],Type, Current) ->
  if
    (Value > Current) -> getMaximalHelper(Tail, Type, Value);
    true -> getMaximalHelper(Tail, Type, Current)
  end;
getMaximalHelper([_|Tail],Type, Current) ->
  getMaximalHelper(Tail, Type, Current).

hourlyMeanHelper([],_,_,Sum,Count) ->
  {Sum, Count};
hourlyMeanHelper([{{_, Hour}, Type, Value}|Tail], Hour, Type, Sum, Count) ->
    hourlyMeanHelper(Tail, Hour, Type, Sum+Value, Count+1);
hourlyMeanHelper([_|Tail], Hour, Type, Sum, Count) ->
    hourlyMeanHelper(Tail, Hour, Type, Sum, Count).

sumAndCountMeasurementsFromDate(_, _, [], _, 0) ->
  null;
sumAndCountMeasurementsFromDate(_, _, [], Sum, Count) ->
  Sum/Count;
sumAndCountMeasurementsFromDate(Date, Type, [{_,_, Measurements}|Tail], Sum, Count) ->
  {I,J} = checkIfStationContainsMeasurement(Date, Type, Measurements, 0, 0),
  sumAndCountMeasurementsFromDate(Date, Type, Tail, Sum+I, Count+J).


checkIfStationContainsMeasurement(_,_,[], Sum, Count) -> {Sum,Count};
checkIfStationContainsMeasurement(Date, Type, [{{Date, _}, Type, Value}|Tail], Sum, Count) ->
   checkIfStationContainsMeasurement(Date, Type, Tail, Sum+Value, Count+1);
checkIfStationContainsMeasurement(Date, Type, [_|Tail], Sum, Count) ->
   checkIfStationContainsMeasurement(Date, Type, Tail, Sum, Count).

doesTableContainStation([],_,_) -> true;
doesTableContainStation([{Name, _, _}|_], Name, _) ->
  false;
doesTableContainStation([{_, Coordinates, _}|_], _, Coordinates) ->
  false;
doesTableContainStation([_|Tail], Name, Coordinates) ->
  doesTableContainStation(Tail, Name, Coordinates).

checkIfValueIsUnique(Beg,Date,Type,Value,[],{Name, Coordinates, []},Tail) ->
  {ok ,Beg ++ [{Name, Coordinates, [{Date, Type, Value}]}] ++ Tail};
checkIfValueIsUnique(Beg,Date,Type,Value,[],{Name, Coordinates, Measurements},Tail) ->
  {ok, Beg ++ [{Name, Coordinates, Measurements ++ [{Date, Type, Value}]}] ++ Tail};
checkIfValueIsUnique(Beg, Date, Type, _, [{Date, Type, _}|_], Station, Tail) ->
  {measurementExists, Beg ++ [Station] ++ Tail};
checkIfValueIsUnique(Beg, Date, Type, Value, [_|MTail], Station, Tail) ->
  checkIfValueIsUnique(Beg, Date, Type, Value, MTail, Station, Tail).

addMeasurement(Beg,_,_,_,{},_) -> {noSuchStation, Beg};
addMeasurement(Beg, Date, Type, Value, {Name, Coordinates, Measurements}, Tail) ->
  checkIfValueIsUnique(Beg, Date, Type, Value, Measurements, {Name, Coordinates, Measurements}, Tail).

delMeasurement(Beg,_,_,_,{},_) -> {noSuchStation, Beg};
delMeasurement(Beg,Date, Type, _, {Name,Coordinates,Table}, Tail) ->
  return_proper_message(Beg, Name, Coordinates, delMeasurementHelper(Date, Type, Table, []),Tail).

return_proper_message(Beg, Name, Coordinates, {Message, Measurement}, Tail) ->
  {Message, Beg ++ [{Name, Coordinates, Measurement}] ++ Tail}.

delMeasurementHelper(_,_,[], Beg) -> {noSuchMeasurement, Beg};
delMeasurementHelper(Date, Type, [{Date,Type, _}|Tail], Beg) ->
  {ok, Beg ++ Tail};
delMeasurementHelper(Date, Type, [Head|Tail], Beg) ->
  delMeasurementHelper(Date, Type, Tail, Beg ++ [Head]).

getMeasurement(_,_,_,_,{},_) -> null;
getMeasurement(_,Date, Type,_, {_, _, Table},_) ->
  getMeasurementHelper(Date, Type, Table).

getMeasurementHelper(_,_, []) -> null;
getMeasurementHelper(Date, Type, [{Date, Type, Value}|_]) ->
  Value;
getMeasurementHelper(Date, Type, [_|Tail]) ->
  getMeasurementHelper(Date, Type, Tail).

meanMeasurements(_,_,_,_,{},_) -> null;
meanMeasurements(_,_,Type,_,{_,_, Table},_) ->
  meanMeasurementsHelper(Type, Table, 0 , 0).

meanMeasurementsHelper(_,[], _, 0) ->
  null;
meanMeasurementsHelper(_,[], Sum, Num) ->
  Sum/Num;
meanMeasurementsHelper(Type, [ {_, Type, Value} |Tail], Sum, Num) ->
  meanMeasurementsHelper(Type, Tail, Sum+Value, Num + 1);
meanMeasurementsHelper(Type, [_|Tail], Sum, Num) ->
  meanMeasurementsHelper(Type, Tail, Sum, Num).

getStation(_,Fun,_,_,_,[],Beg) ->
  Fun(Beg, [], "", "", {}, []);
getStation(Cord, Fun, Date, Type, Value, [{Name,Cord,Measurements}|Tail], Beg) ->
  Fun(Beg,Date, Type, Value, {Name, Cord, Measurements}, Tail);
getStation(Name, Fun, Date, Type, Value, [{Name, Cord, Measurements}|Tail], Beg) ->
  Fun(Beg,Date, Type, Value, {Name, Cord, Measurements}, Tail);
getStation(Identifier, Fun, Date, Type, Value, [Head|Tail], Beg) ->
  getStation(Identifier, Fun, Date, Type, Value, Tail, Beg ++ [Head]).
