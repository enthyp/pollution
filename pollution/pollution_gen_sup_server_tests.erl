%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2019 17:06
%%%-------------------------------------------------------------------
-module(pollution_gen_sup_server_tests).
-author("blob").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {
    setup,
    fun() -> pollution_gen_sup_server:start() end,
    fun(_) ->
      [
        fun test_addStation/0,
        fun test_addValue/0,
        fun test_removeValue/0,
        fun test_getSome/0,
        fun test_getPredictedIndex/0
      ]
    end
  }.


test_addStation() ->
  % add station, check repetitions.
  ?assertEqual(ok, pollution_gen_server:addStation("Station1", {1, 2})),
  ?assertEqual(namerep, pollution_gen_server:addStation("Station1", {2, 3})),
  ?assertEqual(coordrep, pollution_gen_server:addStation("Station2", {1, 2})).

test_addValue() ->
  % add value by name and coords, check errors.
  Date1 = calendar:local_time(),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date1, "PM10", 10)),
  ?assertEqual(ok, pollution_gen_server:addValue({1, 2}, Date1, "PM25", 10)),

  ?assertEqual(badname, pollution_gen_server:addValue("Statdion1", Date1, "PM10", 10)),
  ?assertEqual(badcoord, pollution_gen_server:addValue({9, 10}, Date1, "PM10", 10)),
  ?assertEqual(repmeas, pollution_gen_server:addValue({1, 2}, Date1, "PM10", 20)).

test_removeValue() ->
  % remove value by name and coords, check errors.
  Date1 = calendar:local_time(),
  ?assertEqual(ok, pollution_gen_server:removeValue("Station1", Date1, "PM10")),
  ?assertEqual(ok, pollution_gen_server:removeValue("Station1", Date1, "PM10")),
  ?assertEqual(ok, pollution_gen_server:removeValue({1, 2}, Date1, "PM25")),
  ?assertEqual(badcoord, pollution_gen_server:removeValue({9, 10}, Date1, "PM25")),
  ?assertEqual(badname, pollution_gen_server:removeValue("Stdation1", Date1, "PM10")).

test_getSome() ->
  % get single value, daily mean, station mean.
  Date1 = calendar:local_time(),
  Date2 = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date1) + 10),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date1, "PM10", 10)),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date2, "PM10", 15)),

  ?assertEqual(15, pollution_gen_server:getOneValue("Station1", Date2, "PM10")),
  ?assertEqual(10, pollution_gen_server:getOneValue({1, 2}, Date1, "PM10")),
  ?assertEqual(badname, pollution_gen_server:getOneValue("Statdion1", Date2, "PM10")),
  ?assertEqual(badcoord, pollution_gen_server:getOneValue({9, 10}, Date2, "PM10")),
  ?assertEqual(nomeas, pollution_gen_server:getOneValue("Station1", Date1, "PM25")),

  ?assertEqual(12.5, pollution_gen_server:getStationMean("Station1", "PM10")),
  ?assertEqual(12.5, pollution_gen_server:getStationMean({1, 2}, "PM10")),
  ?assertEqual(badname, pollution_gen_server:getStationMean("Statdion1", "PM10")),
  ?assertEqual(badcoord, pollution_gen_server:getStationMean({9, 10}, "PM10")),
  ?assertEqual(nomeas, pollution_gen_server:getStationMean("Station1", "PM25")).

test_getPredictedIndex() ->
  % get predicted index.
  Date1 = calendar:local_time(),
  Date2 = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date1) - 10),
  Date3 = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date1) - 100000),
  Date4 = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date1) - 100005),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date2, "PM10", 30)),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date3, "PM10", 25)),
  ?assertEqual(verygood, pollution_gen_server:getPredictedIndex({1, 2}, Date1)),

  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date1, "PM25", 40)),
  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date2, "PM25", 50)),
  ?assertEqual(decent, pollution_gen_server:getPredictedIndex({1, 2}, Date1)),

  ?assertEqual(badcoord, pollution_gen_server:getPredictedIndex({2, 2}, Date1)),
  ?assertEqual(nomeas, pollution_gen_server:getPredictedIndex({1, 2}, Date4)),

  ?assertEqual(ok, pollution_gen_server:addValue("Station1", Date4, "PM50", 40)),
  ?assertEqual(undefined, pollution_gen_server:getPredictedIndex({1, 2}, Date4)).