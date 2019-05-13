%%%-------------------------------------------------------------------
%%% @author blob
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. maj 2019 11:51
%%%-------------------------------------------------------------------
-module(pollution_gen_sup_server).
-author("blob").

%% API
-export([start/0]).
-export([init/1]).
-behavior(supervisor).

start() ->
  ?MODULE = ets:new(?MODULE, [set, named_table, public]),
  supervisor:start_link(
    {local, pollution_gen_sup_server},
    pollution_gen_sup_server,
    ok
    ).

init(_) ->
  {ok, {
    {one_for_all, 2, 3}, [
      {
        pollution_gen_server,
        {pollution_gen_server, start, []},
        permanent, brutal_kill, worker, []
      }
    ]
  }}.