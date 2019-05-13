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
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).

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

getOneValue({Lat, Lon}, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, {Lat, Lon}, DateTime, Type});

getOneValue(Name, DateTime, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, Name, DateTime, Type}).

crash() ->
  gen_server:cast(pollution_gen_server, crash).

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
  end.

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
  ok.
