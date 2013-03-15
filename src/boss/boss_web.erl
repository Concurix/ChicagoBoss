-module(boss_web).

-export([start/1, stop/0]).
-export([reload_routes/0,
        reload_translation/1,
        reload_all_translations/0,
        reload_init_scripts/0,
        get_all_routes/0,
        get_all_models/0,
        get_all_applications/0,
        get_all_application_infos/0,
        base_url/1,
        domains/1,
        static_prefix/1,
        translator_config/1,
        router_config/1,
        application_info/1,
        update_info/1]).

-include("boss_web.hrl").

%% Maintain the application state in an ETS table rather than a
%% boss_web_controller process.
-define(BOSS_APPLICATION_TABLE, boss_application_state).

start(AppInfos) ->
    setup_table(),
    set_application_infos(AppInfos).

stop() ->
    teardown_table(),
    ok.

setup_table() ->
    case ets:info(?BOSS_APPLICATION_TABLE) of
        undefined -> ets:new(?BOSS_APPLICATION_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _X -> ets:delete_all_objects(?BOSS_APPLICATION_TABLE)
    end.

teardown_table() ->
    case ets:info(?BOSS_APPLICATION_TABLE) of
        undefined -> ok;
        _X -> ets:delete(?BOSS_APPLICATION_TABLE)
    end.

%% Return the #boss_app_info for a named application or false if
%% the application is unknown.
application_info(AppName) ->
    case ets:lookup(?BOSS_APPLICATION_TABLE, AppName) of
        [{AppName, AppInfo}] ->
            AppInfo;
        _ ->
            false
    end.

%% Insert or overwrite the #boss_app_info for an application.
update_info(#boss_app_info{application=AppName}=AppInfo) ->
    ets:insert(?BOSS_APPLICATION_TABLE, {AppName, AppInfo}).

%% Map a function over every #boss_app_info stored in the table.
map_application_infos(Fun) ->
    lists:map(fun({_Application, Info}) ->
                      Fun(Info)
              end, ets:tab2list(?BOSS_APPLICATION_TABLE)).

%% Execute a function over every #boss_app_info stored in the table,
%% returning the atom 'ok'.
foreach_application_info(Fun) ->
    lists:foreach(fun({_Application, Info}) ->
                          Fun(Info)
                  end, ets:tab2list(?BOSS_APPLICATION_TABLE)).

foldl_application_infos(Fun, Acc) ->
    lists:foldl(fun({_Application, Info}, InnerAcc) ->
                        Fun(Info, InnerAcc)
                end, Acc, ets:tab2list(?BOSS_APPLICATION_TABLE)).

get_all_application_infos() ->
    map_application_infos(fun(AppInfo) -> AppInfo end).

set_application_infos(AppInfos) ->
    lists:foreach(fun(AppInfo) ->
                          update_info(AppInfo)
                  end, AppInfos).

reload_routes() ->
    foreach_application_info(
      fun(#boss_app_info{router_config = RouterConfig}) ->
              boss_router:reload(RouterConfig)
      end).

reload_translation(Locale) ->
    foreach_application_info(
      fun(#boss_app_info{translator_config = TranslatorConfig} = AppInfo) ->
              NewTranslatorConfig = boss_translator:reload(TranslatorConfig, Locale),
              update_info(AppInfo#boss_app_info{translator_config = NewTranslatorConfig})
      end).

reload_all_translations() ->
    foreach_application_info(
      fun(#boss_app_info{translator_config = TranslatorConfig} = AppInfo) ->
              NewTranslatorConfig = boss_translator:reload_all(TranslatorConfig),
              update_info(AppInfo#boss_app_info{translator_config = NewTranslatorConfig})
      end).

reload_init_scripts() ->
    NewAppInfos = map_application_infos(
                    fun(#boss_app_info{application = AppName, init_data = InitData}=AppInfo) ->
                            boss_web_controller:stop_init_scripts(AppName, InitData),
                            NewInitData = boss_web_controller:run_init_scripts(AppName),
                            AppInfo#boss_app_info{init_data = NewInitData}
                    end),
    set_application_infos(NewAppInfos).

get_all_routes() ->
    map_application_infos(
      fun(#boss_app_info{application = AppName, router_config = RouterConfig}) ->
              {AppName, boss_router:get_all(RouterConfig)}
      end).

get_all_models() ->
    foldl_application_infos(
      fun(#boss_app_info{application = AppName}, Acc) ->
              boss_files:model_list(AppName) ++ Acc
      end, []).

get_all_applications() ->
    map_application_infos(fun(#boss_app_info{application = AppName}) -> AppName end).

base_url(App) ->
    case application_info(App) of
        #boss_app_info{base_url = BaseURL} ->
            BaseURL;
        _ ->
            ""
    end.

domains(App) ->
    case application_info(App) of
        #boss_app_info{domains = Domains} ->
            Domains;
        _ ->
            ""
    end.

static_prefix(App) ->
    case application_info(App) of
        #boss_app_info{static_prefix = StaticPrefix} ->
            StaticPrefix;
        _ ->
            ""
    end.

translator_config(AppName) ->
    case application_info(AppName) of
        #boss_app_info{ translator_config = TranslatorConfig } ->
            TranslatorConfig;
        _ ->
            ""
    end.

router_config(AppName) ->
    case application_info(AppName) of
        #boss_app_info{ router_config = RouterConfig } ->
            RouterConfig;
        _ ->
            ""
    end.
