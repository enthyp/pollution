%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. mar 2019 13:39
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("blob").

-include_lib("eunit/include/eunit.hrl").


addStation_test() ->
  Monitor = pollution:createMonitor(),
  Monitor1 = pollution:addStation(Monitor, "Station1", {1, 2}),
  ?assertEqual(pollution:addStation(Monitor1, "Station1", {2, 3}), namerep),
  ?assertEqual(pollution:addStation(Monitor1, "Station2", {1, 2}), coordrep).

addRemoveValue_test() ->
  Date1 = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  Monitor1 = pollution:addStation(Monitor, "Station1", {1, 2}),
  Monitor2 = pollution:addValue(Monitor1, "Station1", Date1, "PM10", 12.2),

  ?assertEqual(pollution:addValue(Monitor2, "Station1", Date1, "PM10", 12.2), repmeas),
  ?assertEqual(pollution:addValue(Monitor2, {1, 2}, Date1, "PM10", 12.2), repmeas),

  Monitor3 = pollution:removeValue(Monitor2, "Station1", Date1, "PM10"),
  pollution:removeValue(Monitor3, "Station12", Date1, "PM10").

getSome_test() ->
  {Date, {Hour, Minute, Second}} = calendar:local_time(),
  Date1 = {Date, {Hour, Minute, Second}},
  Monitor = pollution:createMonitor(),
  Monitor1 = pollution:addStation(Monitor, "Station1", {1, 2}),
  Monitor2 = pollution:addValue(Monitor1, "Station1", Date1, "PM10", 0.2),
  Monitor3 = pollution:addValue(Monitor2, "Station1", {Date, {Hour, Minute, Second + 1}}, "PM10", 2.4),
  Monitor4 = pollution:addValue(Monitor3, {1, 2}, Date1, "PM2.5", 212.2),

  ?assertEqual(pollution:getOneValue(Monitor4, {1, 2}, Date1, "PM2.5"), 212.2),
  ?assertEqual(pollution:getStationMean(Monitor4, "Station1", "PM10"), 1.3),

  Monitor5 = pollution:removeValue(Monitor4, {1, 2}, Date1, "PM2.5"),
  ?assertEqual(pollution:getStationMean(Monitor5, "Station1", "PM2.5"), nomeas),

  Monitor6 = pollution:addStation(Monitor4, "Station2", {2, 4}),
  Monitor7 = pollution:addValue(Monitor6, "Station2", {Date, {Hour, Minute, Second + 1}}, "PM2.5", 212.4),
  ?assertEqual(pollution:getDailyMean(Monitor7, "PM2.5", {2019, 03, 30}), 212.3).
