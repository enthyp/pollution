%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. maj 2019 05:39
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("blob").

%% API
-export([start/0, stop/0, crash/0]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getPredictedIndex/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).

%%% USER INTERFACE

start() ->
  gen_server:start_link(
    {local, pollution_gen_server},
    pollution_gen_server,
    ok,
    []
  ).

stop() ->
  gen_server:cast(pollution_gen_server, stop).

addStation(Name, {Lat, Lon}) ->
  gen_server:call(pollution_gen_server, {addStation, Name, {Lat, Lon}}).

addValue({Lat, Lon}, DateTime, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, {Lat, Lon}, DateTime, Type, Value});

addValue(Name, DateTime, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, Name, DateTime, Type, Value}).

removeValue({Lat, Lon}, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {removeValue, {Lat, Lon}, DateTime, Type});

removeValue(Name, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {removeValue, Name, DateTime, Type}).

getOneValue({Lat, Lon}, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, {Lat, Lon}, DateTime, Type});

getOneValue(Name, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, Name, DateTime, Type}).

getStationMean({Lat, Lon}, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, {Lat, Lon}, Type});

getStationMean(Name, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, Name, Type}).

getDailyMean(Type, Date) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Type, Date}).

getPredictedIndex({Lat, Lon}, DateTime) ->
  gen_server:call(pollution_gen_server, {getPredictedIndex, {Lat, Lon}, DateTime}).

crash() ->
  gen_server:cast(pollution_gen_server, crash).

%%% CALLBACKS

init(_) ->
  case ets:lookup(pollution_gen_sup_server, 1) of
    [] ->
      Monitor = pollution:createMonitor(),
      ets:insert(pollution_gen_sup_server, {1, Monitor});
    [{_, SavedMonitor}] ->
      Monitor = SavedMonitor
  end,
  {ok, Monitor}.

handle_call(Message, From, Monitor) ->
  {reply, Reply, NMonitor} = handle_call_(Message, From, Monitor),
  ets:insert(pollution_gen_sup_server, {1, NMonitor}),
  {reply, Reply, NMonitor}.

handle_call_({addStation, Name, {Lat, Lon}}, _From, Monitor) ->
  case pollution:addStation(Monitor, Name, {Lat, Lon}) of
    namerep ->
      {reply, namerep, Monitor};
    coordrep ->
      {reply, coordrep, Monitor};
    NMonitor ->
      {reply, ok, NMonitor}
  end;

handle_call_({addValue, {Lat, Lon}, DateTime, Type, Value}, _From, Monitor) ->
  case pollution:addValue(Monitor, {Lat, Lon}, DateTime, Type, Value) of
    badcoord ->
      {reply, badcoord, Monitor};
    repmeas ->
      {reply, repmeas, Monitor};
    NMonitor ->
      {reply, ok, NMonitor}
  end;

handle_call_({addValue, Name, DateTime, Type, Value}, _From, Monitor) ->
  case pollution:addValue(Monitor, Name, DateTime, Type, Value) of
    badname ->
      {reply, badname, Monitor};
    repmeas ->
      {reply, repmeas, Monitor};
    NMonitor ->
      {reply, ok, NMonitor}
  end;

handle_call_({removeValue, {Lat, Lon}, DateTime, Type}, _From, Monitor) ->
  case pollution:removeValue(Monitor, {Lat, Lon}, DateTime, Type) of
    badcoord ->
      {reply, badcoord, Monitor};
    NMonitor ->
      {reply, ok, NMonitor}
  end;

handle_call_({removeValue, Name, DateTime, Type}, _From, Monitor) ->
  case pollution:removeValue(Monitor, Name, DateTime, Type) of
    badname ->
      {reply, badname, Monitor};
    NMonitor ->
      {reply, ok, NMonitor}
  end;

handle_call_({getOneValue, {Lat, Lon}, DateTime, Type}, _From, Monitor) ->
  case pollution:getStation(Monitor, {Lat, Lon}) of
    badcoord ->
      {reply, badcoord, Monitor};
    {_, Station} ->
      {reply, pollution:getValueFromStation(Station, DateTime, Type), Monitor}
  end;

handle_call_({getOneValue, Name, DateTime, Type}, _From, Monitor) ->
  case pollution:getStation(Monitor, Name) of
    badname ->
      {reply, badname, Monitor};
    Station ->
      {reply, pollution:getValueFromStation(Station, DateTime, Type), Monitor}
  end;

handle_call_({getStationMean, {Lat, Lon}, Type}, _From, Monitor) ->
  Reply = pollution:getStationMean(Monitor, {Lat, Lon}, Type),
  {reply, Reply, Monitor};

handle_call_({getStationMean, Name, Type}, _From, Monitor) ->
  Reply = pollution:getStationMean(Monitor, Name, Type),
  {reply, Reply, Monitor};

handle_call_({getDailyMean, Type, Date}, _From, Monitor) ->
  Reply = pollution:getDailyMean(Monitor, Type, Date),
  {reply, Reply, Monitor};

handle_call_({getPredictedIndex, {Lat, Lon}, DateTime}, _From, Monitor) ->
  Reply = pollution:getPredictedIndex(Monitor, {Lat, Lon}, DateTime),
  {reply, Reply, Monitor}.

handle_cast(Message, Monitor) ->
  {noreply, NMonitor} = handle_cast_(Message, Monitor),
  ets:insert(pollution_gen_sup_server, {1, NMonitor}),
  {noreply, NMonitor}.

handle_cast_(crash, Monitor) ->
  1 / 0,
  {noreply, Monitor};

handle_cast_(stop, Monitor) ->
  {stop, normal, Monitor}.

terminate(Reason, _) ->
  Reason.
