%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/0, route/1, unroute/3, handle/1, get_all/0, set_controllers/1]).

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    boss_router_controller:start(Options).

stop() ->
    boss_router_controller:stop().

reload() ->
    boss_router_controller:reload().

route(Url) ->
    boss_router_controller:route(Url).

unroute(Controller, Action, Params) ->
    boss_router_controller:unroute(Controller, Action, Params).

handle(StatusCode) ->
    boss_router_controller:handle(StatusCode).

get_all() ->
    boss_router_controllers:get_all().

set_controllers(Controllers) ->
    boss_router_controller:set_controllers(Controllers).
