-module(boss_application_sup).
-author('Concurix, Inc. based on Evan Miller boss_db_sup.erl').

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    Args = [{name, {local, boss_application_pool}},
        {worker_module, boss_application_controller},
        {size, 10}, {max_overflow, 20}|StartArgs],
    PoolSpec = {application_controller, {poolboy, start_link, [Args]}, permanent, 2000, worker, [poolboy]},
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.
