%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/1, route/2, unroute/4, handle/2, get_all/1, set_controllers/2]).

-include("boss_router.hrl").

%% Maintain routing and handler state in ETS tables rather than a
%% boss_router_controller process.
-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    Config = #state{ application = BossApp, routes_table_id = RoutesTableId, 
                     handlers_table_id = HandlersTableId, controllers = Controllers },
    load(Config),
    {ok, Config}.

stop() ->
    ok.

%% reload modifies the tables within a configuration, but does not need to
%% return a new configuration because tables are stateful.
reload(Config) ->
    ets:delete_all_objects(Config#state.routes_table_id),
    ets:delete_all_objects(Config#state.handlers_table_id),
    load(Config),
    ok.

route(Config, "") ->
    route(Config, "/");
route(#state{application = AppName, routes_table_id = RoutesTable}=Config, Url) ->
    case get_match(Url, ets:tab2list(RoutesTable)) of
        undefined -> 
            case string:tokens(Url, "/") of
                [Controller] -> 
                    case is_controller(Config, Controller) of
                        true -> {ok, {AppName, Controller, default_action(Config, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|Params] ->
                    case is_controller(Config, Controller) of
                        true -> {ok, {AppName, Controller, Action, Params}};
                        false -> not_found
                    end;
                _ ->
                    not_found
            end;
        #boss_route{ application = App, controller = C, action = A, params = P } -> 
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end.

unroute(Config, Controller, undefined, Params) ->
    unroute(Config, Controller, default_action(Config, Controller), Params);
unroute(#state{application = AppName, routes_table_id = RoutesTable}, Controller, Action, Params) ->
    RoutedURL = ets:foldl(fun
            (#boss_route{ application = App, controller = C, action = A, params = P } = Route, Default) 
                when App =:= AppName, C =:= Controller, A =:= Action ->
                case lists:keysort(1, Params) =:= lists:keysort(1, P) of
                    true ->
                        Route#boss_route.url;
                    false ->
                        Default
                end;
            (_, Default) ->
                Default
        end, undefined, RoutesTable),
    case RoutedURL of
        undefined ->
            ControllerModule = list_to_atom(boss_files:web_controller(AppName, Controller)),
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
    end.

handle(Config, StatusCode) ->
    case ets:lookup(Config#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end.

get_all(Config) ->
    lists:map(
      fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
              [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
      end, lists:flatten(ets:match(Config#state.routes_table_id, '$1'))).

%% set_controllers returns a modified configuration, which should be retained
%% by the caller.
set_controllers(Config, ControllerList) ->
    Config#state{ controllers = ControllerList }.

%% Internal functions

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, OrderedRoutes} -> 
            Routes = lists:zipwith(fun(Number, {Url, Proplist}) -> {Number, Url, Proplist} end, lists:seq(1,length(OrderedRoutes)), OrderedRoutes),
            lists:map(fun
                    ({Number, UrlOrStatusCode, Proplist}) when is_list(Proplist) ->
                        TheApplication = proplists:get_value(application, Proplist, State#state.application),
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
                                true = ets:insert(State#state.routes_table_id, NewRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.handlers_table_id, NewHandler)
                        end
                end, Routes);
        Error -> 
            error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
    end.

is_controller(State, Controller) -> 
    lists:member(boss_files:web_controller(State#state.application, Controller), State#state.controllers).

default_action(State, Controller) ->
    case is_controller(State, Controller) of
        true ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, Controller)),
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
