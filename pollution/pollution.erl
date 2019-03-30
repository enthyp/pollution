%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2019 12:42
%%%-------------------------------------------------------------------
-module(pollution).
-author("blob").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

-record(station, {typeDate2meas=#{}}).
-record(monitor, {coords2name=#{}, name2station=#{}}).


%%%
createMonitor() ->
  #monitor{}.


%%%
addStation(Monitor, Name, {Lat, Lon}) -> % guard?
  Coords2name = Monitor#monitor.coords2name,
  Name2station = Monitor#monitor.name2station,

  case {maps:is_key({Lat, Lon}, Coords2name), maps:is_key(Name, Name2station)} of
    {false, false} ->
      NCoords2name = Coords2name#{{Lat, Lon} => Name},
      NName2station = Name2station#{Name => #station{}},
      Monitor#monitor{coords2name = NCoords2name, name2station = NName2station};
    {false, true} ->
      namerep;
    {_, _} ->
      coordrep
  end.


%%% Helpers %%%
getStation(Monitor, {Lat, Lon}) ->
  Coords2name = Monitor#monitor.coords2name,
  Name2station = Monitor#monitor.name2station,

  case maps:get({Lat, Lon}, Coords2name, badkey) of
    badkey ->
      badcoord;
    Name ->
      {Name, maps:get(Name, Name2station, badname)}
  end;

getStation(Monitor, Name) ->
  Name2station = Monitor#monitor.name2station,
  maps:get(Name, Name2station, badname).


%%%
addValue(Monitor, {Lat, Lon}, Date, Type, Value) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {Name, Station} ->
      addValueToStation(Monitor, Name, Station, Date, Type, Value)
  end;

addValue(Monitor, Name, Date, Type, Value) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      addValueToStation(Monitor, Name, Station, Date, Type, Value)
  end.

addValueToStation(Monitor, Name, Station, Date, Type, Value) ->
  TypeDate2meas = Station#station.typeDate2meas,

  case maps:get({Date, Type}, TypeDate2meas, badkey) of
    badkey ->
      NTypeDate2meas = TypeDate2meas#{{Date, Type} => Value},
      NStation = Station#station{typeDate2meas = NTypeDate2meas},
      NName2station = (Monitor#monitor.name2station)#{Name => NStation},
      Monitor#monitor{name2station = NName2station};
    _ ->
      repmeas
  end.


%%%
removeValue(Monitor, {Lat, Lon}, Date, Type) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {Name, Station} ->
      removeValueFromStation(Monitor, Name, Station, Date, Type)
  end;

removeValue(Monitor, Name, Date, Type) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      removeValueFromStation(Monitor, Name, Station, Date, Type)
  end.

removeValueFromStation(Monitor, Name, Station, Date, Type) ->
  TypeDate2meas = Station#station.typeDate2meas,
  NTypeDate2meas = maps:remove({Date, Type}, TypeDate2meas),

  NStation = Station#station{typeDate2meas = NTypeDate2meas},
  NName2station = (Monitor#monitor.name2station)#{Name => NStation},
  Monitor#monitor{name2station = NName2station}.


%%%
getOneValue(Monitor, {Lat, Lon}, Date, Type) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {_, Station} ->
      getValueFromStation(Station, Date, Type)
  end;

getOneValue(Monitor, Name, Date, Type) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    {_, Station} ->
      getValueFromStation(Station, Date, Type)
  end.

getValueFromStation(Station, Date, Type) ->
  TypeDate2meas = Station#station.typeDate2meas,
  maps:get({Date, Type}, TypeDate2meas, undefined).


%%%
getStationMean(Monitor, {Lat, Lon}, Type) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {_, Station} ->
      getMean(Station, Type)
  end;

getStationMean(Monitor, Name, Type) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      getMean(Station, Type)
  end.

getMean(Station, Type) ->
  TypeDate2meas = Station#station.typeDate2meas,
  Measurements = maps:values(maps:filter(fun(K, V) -> filterType(K, V, Type) end, TypeDate2meas)),

  case Measurements == [] of
    true ->
      nomeas;
    false ->
      lists:sum(Measurements) / length(Measurements)
  end.

filterType(K, _, Type) ->
  case K of
    {_, Type} ->
      true;
    _ ->
      false
  end.


%%%
getDailyMean(Monitor, Type, Date) ->
  Stations = maps:values(Monitor#monitor.name2station),
  TypeDate2measMaps = [Station#station.typeDate2meas || Station <- Stations],
  TypeDate2meas = lists:foldl(fun maps:merge/2, #{}, TypeDate2measMaps),
  Measurements = maps:values(maps:filter(fun(K, V) -> filterTypeDay(K, V, Type, Date) end, TypeDate2meas)),
  case Measurements == [] of
    true ->
      nomeas;
    false ->
      lists:sum(Measurements) / length(Measurements)
  end.

filterTypeDay(K, _, Type, Date) ->
  case K of
    {{Date, _}, Type} ->
      true;
    _ ->
      false
  end.