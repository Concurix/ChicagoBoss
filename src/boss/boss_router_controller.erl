-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("boss_router.hrl").

-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp, routes_table_id = RoutesTableId, 
        handlers_table_id = HandlersTableId, controllers = Controllers },
    boss_router:load(State),
    {ok, State}.

handle_call(reload, _From, State) ->
    boss_router:reload(State),
    {reply, ok, State};
handle_call({handle, StatusCode}, _From, State) ->
    Result = boss_router:handle(State, StatusCode),
    {reply, Result, State};
handle_call({route, ""}, From, State) ->
    handle_call({route, "/"}, From, State);
handle_call({route, Url}, _From, State) ->
    Route = boss_router:route(State, Url),
    {reply, Route, State};
handle_call({unroute, Controller, undefined, Params}, From, State) ->
    handle_call({unroute, Controller, boss_router:default_action(State, Controller), Params}, From, State);
handle_call({unroute, Controller, Action, Params}, _From, State) ->
    Result = boss_router:unroute(State, Controller, Action, Params),
    {reply, Result, State};
handle_call(get_all, _From, State) ->
    Res = boss_router:get_all(State),
    {reply, Res, State};
handle_call({set_controllers, ControllerList}, _From, State) ->
    {reply, ok, State#state{ controllers = ControllerList }}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.routes_table_id),
    ets:delete(State#state.handlers_table_id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

