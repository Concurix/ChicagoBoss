%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/1]).
-export([reload/1, route/2, unroute/5, unroute_lookup/4, handle/2, get_all/1, set_controllers/2]).

-include("boss_router.hrl").

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    ReverseRoutesTableId = ets:new(?BOSS_REVERSE_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp,
                    routes_table_id = RoutesTableId, 
                    reverse_routes_table_id = ReverseRoutesTableId,
                    handlers_table_id = HandlersTableId,
                    controllers = Controllers },
    load(State),
    {ok, State}.

stop(State) ->
    ets:delete(State#state.routes_table_id),
    ets:delete(State#state.reverse_routes_table_id),
    ets:delete(State#state.handlers_table_id),
    ok.

%% reload modifies the tables within a configuration, but does not need to
%% return a new configuration because tables are stateful.
reload(State) ->
    ets:delete_all_objects(State#state.routes_table_id),
    ets:delete_all_objects(State#state.reverse_routes_table_id),
    ets:delete_all_objects(State#state.handlers_table_id),
    load(State),
    ok.

route(State, "") ->
    route(State, "/");
route(#state{application = AppName, routes_table_id = RoutesTable}=State, Url) ->
    case get_match(Url, ets:tab2list(RoutesTable)) of
        undefined -> 
            case string:tokens(Url, "/") of
                [Controller] -> 
                    case is_controller(State, Controller) of
                        true -> {ok, {AppName, Controller, default_action(State, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|Tokens] ->
                    case is_controller(State, Controller) of
                        true ->
                            UnquotedTokens = lists:map(fun mochiweb_util:unquote/1, Tokens),
                            {ok, {AppName, Controller, Action, UnquotedTokens}};
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

%% Perform a quick lookup in the reverse routes table.
unroute_lookup(State, Controller, undefined, Params) ->
    unroute_lookup(State, Controller, default_action(State, Controller), Params);
unroute_lookup(#state{application = AppName, reverse_routes_table_id = ReverseRoutesTable}, Controller, Action, Params) ->
    case ets:lookup(ReverseRoutesTable,
                    {AppName, Controller, Action, lists:keysort(1, Params)}) of
        [#boss_reverse_route{ url = Url }] -> Url;
        [] -> undefined
    end.

%% Compute a URL from an application, controller, action and set of parameters.
%% Do so manually if a quick check of the reverse routes table fails.
unroute(State, Application, Controller, undefined, Params) ->
    unroute(State, Application, Controller, default_action(State, Controller), Params);
unroute(State, Application, Controller, Action, Params) ->
    case unroute_lookup(State, Controller, Action, Params) of
        undefined ->
            ControllerModule = list_to_atom(boss_files:web_controller(Application, Controller)),
            {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, ControllerModule, list_to_atom(Action)),

            URL = case Tokens of
                [] ->
                    lists:concat(["/", Controller, "/", Action]);
                _ ->
                    lists:concat(["/", Controller, "/", Action |
                            lists:foldr(fun(T, Acc) -> ["/", mochiweb_util:quote_plus(T) | Acc] end, [], Tokens)])
            end,
            QueryString = mochiweb_util:urlencode(Variables1),
            case QueryString of
                "" ->
                    URL;
                _ ->
                    URL ++ "?" ++ QueryString
            end;
        RoutedURL ->
            RoutedURL
    end.

handle(State, StatusCode) ->
    case ets:lookup(State#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end.

get_all(State) ->
    lists:map(
      fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
              [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
      end, lists:flatten(ets:match(State#state.routes_table_id, '$1'))).

%% set_controllers returns a modified configuration, which should be retained
%% by the caller.
set_controllers(State, ControllerList) ->
    State#state{ controllers = ControllerList }.

%% Internal functions

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, OrderedRoutes} -> 
            lists:foldl(fun
                    ({UrlOrStatusCode, Proplist}, Number) when is_list(Proplist) ->
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
                                NewReverseRoute = #boss_reverse_route{
                                    application_controller_action_params = {
                                        TheApplication,
                                        TheController,
                                        TheAction,
                                        lists:keysort(1, CleanParams)
                                    },
                                    url = Url
                                },
                                true = ets:insert(State#state.routes_table_id, NewRoute),
                                true = ets:insert(State#state.reverse_routes_table_id, NewReverseRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.handlers_table_id, NewHandler)
                        end,
                        Number+1
                end, 1, OrderedRoutes);
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
