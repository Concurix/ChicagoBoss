%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/1, route/2, unroute/4, handle/2, get_all/1, set_controllers/2]).

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    boss_router_controller:start(Options).

stop() ->
    boss_router_controller:stop().

reload(Config) ->
    boss_router_controller:reload(Config).

route(Config, Url) ->
    boss_router_controller:route(Config, Url).

unroute(Config, Controller, Action, Params) ->
    boss_router_controller:unroute(Config, Controller, Action, Params).

handle(Config, StatusCode) ->
    boss_router_controller:handle(Config, StatusCode).

get_all(Config) ->
    boss_router_controllers:get_all(Config).

set_controllers(Config, Controllers) ->
    boss_router_controller:set_controllers(Config, Controllers).
