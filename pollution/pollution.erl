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
-export([getPredictedIndex/3]).
-export([getValueFromStation/3, getStation/2]).

-record(station, {typeDate2meas=#{}}).
-record(monitor, {coords2name=#{}, name2station=#{}}).

-include_lib("eunit/include/eunit.hrl"). % for debugging only

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
addValue(Monitor, {Lat, Lon}, DateTime, Type, Value) -> % Date guard?
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {Name, Station} ->
      addValueToStation(Monitor, Name, Station, DateTime, Type, Value)
  end;

addValue(Monitor, Name, DateTime, Type, Value) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      addValueToStation(Monitor, Name, Station, DateTime, Type, Value)
  end.

addValueToStation(Monitor, Name, Station, DateTime, Type, Value) ->
  TypeDate2meas = Station#station.typeDate2meas,

  case maps:is_key({DateTime, Type}, TypeDate2meas) of
    false ->
      NTypeDate2meas = TypeDate2meas#{{DateTime, Type} => Value},
      NStation = Station#station{typeDate2meas = NTypeDate2meas},
      NName2station = (Monitor#monitor.name2station)#{Name => NStation},
      Monitor#monitor{name2station = NName2station};
    true ->
      repmeas
  end.


%%%
removeValue(Monitor, {Lat, Lon}, DateTime, Type) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {Name, Station} ->
      removeValueFromStation(Monitor, Name, Station, DateTime, Type)
  end;

removeValue(Monitor, Name, DateTime, Type) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      removeValueFromStation(Monitor, Name, Station, DateTime, Type)
  end.

removeValueFromStation(Monitor, Name, Station, DateTime, Type) ->
  TypeDate2meas = Station#station.typeDate2meas,
  NTypeDate2meas = maps:remove({DateTime, Type}, TypeDate2meas),

  NStation = Station#station{typeDate2meas = NTypeDate2meas},
  NName2station = (Monitor#monitor.name2station)#{Name => NStation},
  Monitor#monitor{name2station = NName2station}.


%%%
getOneValue(Monitor, {Lat, Lon}, DateTime, Type) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {_, Station} ->
      getValueFromStation(Station, DateTime, Type)
  end;

getOneValue(Monitor, Name, DateTime, Type) ->
  case getStation(Monitor, Name) of
    badname ->
      badname;
    Station ->
      getValueFromStation(Station, DateTime, Type)
  end.

getValueFromStation(Station, DateTime, Type) ->
  TypeDate2meas = Station#station.typeDate2meas,
  maps:get({DateTime, Type}, TypeDate2meas, nomeas).


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
  Measurements = maps:values(maps:filter(fun(K, V) -> filterTypeDate(K, V, Type, Date) end, TypeDate2meas)),
  case Measurements == [] of
    true ->
      nomeas;
    false ->
      lists:sum(Measurements) / length(Measurements)
  end.

filterTypeDate(K, _, Type, Date) ->
  case K of
    {{Date, _}, Type} ->
      true;
    _ ->
      false
  end.


%%% what index??
getPredictedIndex(Monitor, {Lat, Lon}, DateTime) ->
  case getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      badcoord;
    {_, Station} ->
      TypeDate2meas = Station#station.typeDate2meas,
      Measurements = maps:filter(fun(K, V) -> filterDate(K, V, DateTime) end, TypeDate2meas),
      case maps:size(Measurements) of
        0 ->
          nomeas;
        _ ->
          getIndex(Measurements)
      end
  end.

filterDate(K, _, TargetDateTime) ->
    {DateTime, _} = K,
    Seconds = lists:map(fun calendar:datetime_to_gregorian_seconds/1, [DateTime, TargetDateTime]),
    Diff = lists:foldl(fun (El, Acc) -> El - Acc end, 0, Seconds),
    ((Diff >= 0) and (Diff =< 86400)).

% example implementation
getIndex(Measurements) ->
  IndicesMap = #{0 => undefined, 1 => verygood, 2 => good, 3 => decent, 4 => mediocre, 5 => bad, 6 => verybad},
  ThresholdsMap = #{"PM10" => [20, 60, 100, 140, 200],
                    "PM25" => [12, 36, 60, 84, 120]},
  % add more if you like

  Type2Measurements = maps:fold(fun groupMeasurements/3, #{}, Measurements),
  Type2Mean = maps:map(fun (_, V) -> lists:sum(V) / length(V) end, Type2Measurements),
  Type2Index = maps:map(fun (K, V) -> getIndexByThreshold(V, maps:get(K, ThresholdsMap, nothresh), 1) end, Type2Mean),
  maps:get(lists:max(maps:values(Type2Index)), IndicesMap).

groupMeasurements({_, Type}, V, Acc) ->
  Values = maps:get(Type, Acc, []),
  Acc#{Type => [V | Values]}.

getIndexByThreshold(_, nothresh, _) ->
  0;

getIndexByThreshold(_, [], Index) ->
  Index;

getIndexByThreshold(Value, [H | T], Index) ->
  case Value =< H of
    true ->
      Index;
    false ->
      getIndexByThreshold(Value, T, Index + 1)
  end.
