%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 11:42
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("blob").

%% API
-export([start/0]).

start() ->
  register(supervisor, spawn(fun supervise/0)).

supervise() ->
  process_flag(trap_exit, true),
  register(pollutionServer, spawn_link(pollution_server, init, [])),
  io:format("Supervising ~p~n", [whereis(pollutionServer)]),
  receive
    {'EXIT', _, _} ->
      io:format("Pollution server crashed!~nRestarting...~n", []),
      supervise();
    stop ->
      io:format("Stopping..."),
      ok
  end.