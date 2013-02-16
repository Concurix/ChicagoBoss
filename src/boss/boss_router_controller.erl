-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("boss_router.hrl").

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    boss_router:start(Options).

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
    Result = boss_router:unroute_lookup(State, Controller, Action, Params),
    {reply, Result, State};
handle_call(get_all, _From, State) ->
    Res = boss_router:get_all(State),
    {reply, Res, State};
handle_call({set_controllers, ControllerList}, _From, State) ->
    {reply, ok, boss_router:set_controllers(State, ControllerList)}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    boss_router:stop(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

