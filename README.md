# Erlang Pollution Server OTP
Simple project wirtten in Erlang. It contains four modules:
* pollution
* gen_server
* server_supervisor
* pollution_value_collector_gen_statem
### Module pollution:
It contains functionalities useful while storing data from weather stations.
### Module gen_server:
It contains pollution server functionality.
### Module server_supervisor
It restarts *gen_server* module or *pollution_value_collector_gen_statem* module if they crash.
### pollution_value_collector_gen_statem:
A module that implements a state machine, allowing to insert many Measurements to a selected station. If proper order of operations is not preserved, the module crashes and is being restarted by server_supervisor.

-----

## gen_server available functions:
# 1. addStation
```
addStation(Name, Coordinates)
```
Function is adding station with *Name* and *Coordinates*.
* *Name* must be distinctive
* *Coordinates* must be disctinctive

# 2. addValue
```
addValue(StationIdentifier, MeasurementDate, MeasurementType, MeasurementValue)
```
Function is adding Measurement build by *MeasurementDate*, *MeasurementType* and *MeasurementValue*.
*StationIdentifier* is a *Name* or a *Coordinate* of a station.

# 3. removeValue
```
removeValue(StationIdentifier, MeasurementDate, MeasurementType)
```
Deletes Measurement from station with *StationIdentifier* with *MeasurementDate* and *MeasurementType*.

# 4. getOneValue
```
getOneValue(StationIdentifier, MeasurementDate, MeasurementType)
```
Gets a value of the Measurement that has *MeasurementDate* and *MeasurementType* from station with *StationIdentifier*.

# 5. getStationMean
```
getStationMean(StationIdentifier, MeasurementType)
```
Gets mean value of the Measurements that has *MeasurementType* from station with *StationIdentifier*.

# 6. getDailyMean
```
getDailyMean(Date, Type)
```
Gets mean value of the Measurements that has *Date* and *Type*

# 7. getHourlyMean
```
getHourlyMean(StationIdentifier, Hour, Type)
```
Gets mean value of the Measurements that has *Hour* and *Type* from *StationIdentifier*

# 8. getLastMeasurement
```
getLastMeasurement(StationIdentifier, Type)
```
Gets last value of the Measurements that has *Type* from *StationIdentifier*

# 9. getMeasureMaxValue
```
getMeasureMaxValue(StationIdentifier, Type)
```
Gets biggest value of the Measurements that has *Type* from *StationIdentifier*

## pollution_value_collector_gen_statem available functions:
# 1. set_station
```
set_station(Name, Coordinates)
```
Begins the operation, can not be used after *add_value*. Creates new *Station* with *Name* and *Coordinates* or finds *Station* with *Name* if *Station* with given *Name* exists.
# 2. add_value
```
add_value(Date, Type, Value)
```
Adds new *Measurement* to *Measurement* table. Can not be used as the first operation and can't be used after using *store_data*.
# 3. store_data
```
store_data()
```
Saves all *Measurements* saved in *Measurement* table. Can not be used as the first operation and can not be used after *store_data*.
