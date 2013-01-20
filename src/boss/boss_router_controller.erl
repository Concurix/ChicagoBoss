-module(boss_router_controller).

-export([start/1, stop/0]).

-export([terminate/2]).
-export([reload/1, handle/2, route/2, unroute/4, get_all/1, set_controllers/2]).

-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).
-record(boss_route, {number, url, pattern, application, controller, action, params = []}).
-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(boss_router_config, {
        application,
        controllers = [],
        routes_table_id,
        handlers_table_id
    }).

start(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    Config = #boss_router_config{ application = BossApp, routes_table_id = RoutesTableId, 
        handlers_table_id = HandlersTableId, controllers = Controllers },
    load(Config),
    {ok, Config}.

stop() ->
    ok.

%% reload modifies the tables within a configuration, but does not need to
%% return a new configuration because tables are stateful.
reload(Config) ->
    ets:delete_all_objects(Config#boss_router_config.routes_table_id),
    ets:delete_all_objects(Config#boss_router_config.handlers_table_id),
    load(Config),
    ok.

handle(Config, StatusCode) ->
    Result = case ets:lookup(Config#boss_router_config.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    Result.
   
route(Config, "") ->
    route(Config, "/");
route(Config, Url) ->
    Route = case get_match(Url, ets:tab2list(Config#boss_router_config.routes_table_id)) of
        undefined -> 
            case string:tokens(Url, "/") of
                [Controller] -> 
                    case is_controller(Config, Controller) of
                        true -> {ok, {Config#boss_router_config.application, Controller, default_action(Config, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|Params] ->
                    case is_controller(Config, Controller) of
                        true -> {ok, {Config#boss_router_config.application, Controller, Action, Params}};
                        false -> not_found
                    end;
                _ ->
                    not_found
            end;
        #boss_route{ application = App, controller = C, action = A, params = P } -> 
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    Route.

unroute(Config, Controller, undefined, Params) ->
    unroute(Config, Controller, default_action(Config, Controller), Params);
unroute(Config, Controller, Action, Params) ->
    RoutedURL = ets:foldl(fun
            (#boss_route{ application = App, controller = C, action = A, params = P } = Route, Default) 
                when App =:= Config#boss_router_config.application, C =:= Controller, A =:= Action ->
                case lists:keysort(1, Params) =:= lists:keysort(1, P) of
                    true ->
                        Route#boss_route.url;
                    false ->
                        Default
                end;
            (_, Default) ->
                Default
        end, undefined, Config#boss_router_config.routes_table_id),
    Result = case RoutedURL of
        undefined ->
            ControllerModule = list_to_atom(boss_files:web_controller(Config#boss_router_config.application, Controller)),
            {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, ControllerModule, list_to_atom(Action)),

            URL = case Tokens of
                [] ->
                    lists:concat(["/", Controller, "/", Action]);
                _ ->
                    lists:concat(["/", Controller, "/", Action |
                            lists:foldr(fun(T, Acc) -> ["/", T | Acc] end, [], Tokens)])
            end,
            QueryString = mochiweb_util:urlencode(Variables1),
            case QueryString of
                "" ->
                    URL;
                _ ->
                    URL ++ "?" ++ QueryString
            end;
        _ ->
            RoutedURL
    end,
    Result.
    
get_all(Config) ->
    Res = lists:map(fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
                [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
        end, lists:flatten(ets:match(Config#boss_router_config.routes_table_id, '$1'))),
    Res.

%% set_controllers returns a modified configuration, which should be retained
%% by the caller.
set_controllers(Config, ControllerList) ->
    Config#boss_router_config{ controllers = ControllerList }.

terminate(_Reason, Config) ->
    ets:delete(Config#boss_router_config.routes_table_id),
    ets:delete(Config#boss_router_config.handlers_table_id).

load(Config) ->
    RoutesFile = boss_files:routes_file(Config#boss_router_config.application),
    error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, OrderedRoutes} -> 
            Routes = lists:zipwith(fun(Number, {Url, Proplist}) -> {Number, Url, Proplist} end, lists:seq(1,length(OrderedRoutes)), OrderedRoutes),
            lists:map(fun
                    ({Number, UrlOrStatusCode, Proplist}) when is_list(Proplist) ->
                        TheApplication = proplists:get_value(application, Proplist, Config#boss_router_config.application),
                        TheController = proplists:get_value(controller, Proplist),
                        TheAction = proplists:get_value(action, Proplist),
                        CleanParams = lists:foldl(fun(Key, Vars) ->
                                    proplists:delete(Key, Vars)
                            end, Proplist, [application, controller, action]),
                        case UrlOrStatusCode of
                            Url when is_list(Url) ->
                                {ok, MP} = re:compile("^"++Url++"$"),
                                NewRoute = #boss_route{ 
                                    number = Number,
                                    url = Url, 
                                    pattern = MP,
                                    application = TheApplication, 
                                    controller = TheController, 
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(Config#boss_router_config.routes_table_id, NewRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(Config#boss_router_config.handlers_table_id, NewHandler)
                        end
                end, Routes);
        Error -> 
            error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
    end.

is_controller(Config, Controller) -> 
    lists:member(boss_files:web_controller(Config#boss_router_config.application, Controller), Config#boss_router_config.controllers).

default_action(Config, Controller) ->
    case is_controller(Config, Controller) of
        true ->
            ControllerModule = list_to_atom(boss_files:web_controller(Config#boss_router_config.application, Controller)),
            case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                [DefaultAction] when is_atom(DefaultAction) ->
                    atom_to_list(DefaultAction);
                _ ->
                    "index"
            end;
        false ->
            "index"
    end.

substitute_params(Params, Matches) ->
    substitute_params(Params, Matches, []).

substitute_params([], _Matches, FinalParams) ->
    lists:reverse(FinalParams);
substitute_params([{Key, Value}|Rest], Matches, FinalParams) when is_integer(Value) ->
    substitute_params(Rest, Matches, [{Key, lists:nth(Value, Matches)}|FinalParams]);
substitute_params([{Key, Value}|Rest], Matches, FinalParams) ->
    substitute_params(Rest, Matches, [{Key, Value}|FinalParams]).

get_match(_, []) ->
    undefined;
get_match(Url, [Route = #boss_route{pattern = MP}|T]) ->
    Params = Route#boss_route.params,
    {IndexedParams, Vars} = lists:mapfoldr(fun
            ({Key, Value}, Acc) when is_atom(Value) ->
                case atom_to_list(Value) of
                    [$$, C | Rest] when C >= $0, C =< $9 ->
                        {{Key, length(Acc)+1}, [list_to_integer([C|Rest])|Acc]};
                    "$"++VarName ->
                        {{Key, length(Acc)+1}, [VarName|Acc]};
                    _ ->
                        {{Key, Value}, Acc}
                end;
            ({Key, Value}, Acc) ->
                {{Key, Value}, Acc}
        end, [], Params),
    case re:run(Url, MP, [{capture, lists:reverse(Vars), list}]) of
        {match, Matches} ->
            Route#boss_route{ params = substitute_params(IndexedParams, Matches) };
        match ->
            Route;
        _ ->
            get_match(Url, T)
    end.
