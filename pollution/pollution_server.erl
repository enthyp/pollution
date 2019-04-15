%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2019 13:49
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("blob").

-include_lib("eunit/include/eunit.hrl").


%% API
-export([start/0, stop/0]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getPredictedIndex/2]).

start() ->
  register(pollutionServer, spawn(fun init/0)).

stop() ->
  call(stop).

% public API
call(Message) ->
  pollutionServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, {Lat, Lon}) -> call({addStation, Name, {Lat, Lon}}).
addValue({Lat, Lon}, DateTime, Type, Value) -> call({addValue, {Lat, Lon}, DateTime, Type, Value});
addValue(Name, DateTime, Type, Value) -> call({addValue, Name, DateTime, Type, Value}).
removeValue({Lat, Lon}, DateTime, Type) -> call({removeValue, {Lat, Lon}, DateTime, Type});
removeValue(Name, DateTime, Type) -> call({removeValue, Name, DateTime, Type}).
getOneValue({Lat, Lon}, DateTime, Type) -> call({getOneValue, {Lat, Lon}, DateTime, Type});
getOneValue(Name, DateTime, Type) -> call({getOneValue, Name, DateTime, Type}).
getStationMean({Lat, Lon}, Type) -> call({getStationMean, {Lat, Lon}, Type});
getStationMean(Name, Type) -> call({getStationMean, Name, Type}).
getDailyMean(Type, Date) -> call({getDailyMean, Type, Date}).
getPredictedIndex({Lat, Lon}, DateTime) -> call({getPredictedIndex, {Lat, Lon}, DateTime}).

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {request, Pid, {addStation, Name, {Lat, Lon}}} ->
      addStationIn(Pid, Monitor, Name, {Lat, Lon});
    {request, Pid, {addValue, {Lat, Lon}, DateTime, Type, Value}} ->
      addValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type, Value);
    {request, Pid, {addValue, Name, DateTime, Type, Value}} ->
      addValueIn(Pid, Monitor, Name, DateTime, Type, Value);
    {request, Pid, {removeValue, {Lat, Lon}, DateTime, Type}} ->
      removeValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type);
    {request, Pid, {removeValue, Name, DateTime, Type}} ->
      removeValueIn(Pid, Monitor, Name, DateTime, Type);
    {request, Pid, {getOneValue,  {Lat, Lon}, DateTime, Type}} ->
      getOneValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type);
    {request, Pid, {getOneValue,  Name, DateTime, Type}} ->
      getOneValueIn(Pid, Monitor, Name, DateTime, Type);
    {request, Pid, {getStationMean, {Lat, Lon}, Type}} ->
      getStationMeanIn(Pid, Monitor, {Lat, Lon}, Type);
    {request, Pid, {getStationMean, Name, Type}} ->
      getStationMeanIn(Pid, Monitor, Name, Type);
    {request, Pid, {getDailyMean, Type, Date}} ->
      getDailyMeanIn(Pid, Monitor, Type, Date);
    {request, Pid, {getPredictedIndex, {Lat, Lon}, DateTime}} ->
      getPredictedIndexIn(Pid, Monitor, {Lat, Lon}, DateTime);
    {request, Pid, stop} ->
      Pid ! {reply, ok}
  end.

addStationIn(Pid, Monitor, Name, {Lat, Lon}) ->
  case pollution:addStation(Monitor, Name, {Lat, Lon}) of
    namerep ->
      Pid ! {reply, namerep},
      loop(Monitor);
    coordrep ->
      Pid ! {reply, coordrep},
      loop(Monitor);
    NMonitor ->
      Pid ! {reply, ok},
      loop(NMonitor)
  end.

addValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type, Value) ->
  case pollution:addValue(Monitor, {Lat, Lon}, DateTime, Type, Value) of
    badcoord ->
      Pid ! {reply, badcoord},
      loop(Monitor);
    repmeas ->
      Pid ! {reply, repmeas},
      loop(Monitor);
    NMonitor ->
      Pid ! {reply, ok},
      loop(NMonitor)
  end;

addValueIn(Pid, Monitor, Name, DateTime, Type, Value) ->
  case pollution:addValue(Monitor, Name, DateTime, Type, Value) of
    badname ->
      Pid ! {reply, badname},
      loop(Monitor);
    repmeas ->
      Pid ! {reply, repmeas},
      loop(Monitor);
    NMonitor ->
      Pid ! {reply, ok},
      loop(NMonitor)
  end.

removeValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type) ->
  case pollution:removeValue(Monitor, {Lat, Lon}, DateTime, Type) of
    badcoord ->
      Pid ! {reply, badcoord},
      loop(Monitor);
    NMonitor ->
      Pid ! {reply, ok},
      loop(NMonitor)
  end;

removeValueIn(Pid, Monitor, Name, DateTime, Type) ->
  case pollution:removeValue(Monitor, Name, DateTime, Type) of
    badname ->
      Pid ! {reply, badname},
      loop(Monitor);
    NMonitor ->
      Pid ! {reply, ok},
      loop(NMonitor)
  end.

getOneValueIn(Pid, Monitor, {Lat, Lon}, DateTime, Type) ->
  Result = pollution:getOneValue(Monitor, {Lat, Lon}, DateTime, Type),
  Pid ! {reply, Result},
  loop(Monitor);

getOneValueIn(Pid, Monitor, Name, DateTime, Type) ->
  Result = pollution:getOneValue(Monitor, Name, DateTime, Type),
  Pid ! {reply, Result},
  loop(Monitor).

getStationMeanIn(Pid, Monitor, {Lat, Lon}, Type) ->
  Result = pollution:getStationMean(Monitor, {Lat, Lon}, Type),
  Pid ! {reply, Result},
  loop(Monitor);

getStationMeanIn(Pid, Monitor, Name, Type) ->
  Result = pollution:getStationMean(Monitor, Name, Type),
  Pid ! {reply, Result},
  loop(Monitor).

getDailyMeanIn(Pid, Monitor, Type, Date) ->
  Result = pollution:getDailyMean(Monitor, Type, Date),
  Pid ! {reply, Result},
  loop(Monitor).

getPredictedIndexIn(Pid, Monitor, {Lat, Lon}, DateTime) ->
  Result = pollution:getPredictedIndex(Monitor, {Lat, Lon}, DateTime),
  Pid ! {reply, Result},
  loop(Monitor).