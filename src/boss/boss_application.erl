%% @doc Manage application state for the web controller process.

-module(boss_application).

-export([start/1, stop/0,
         reload_routes/0, reload_translation/1, reload_all_translations/0,
         reload_init_scripts/0, get_all_routes/0, get_all_models/0, get_all_applications/0,
         set_application_infos/1, get_all_application_infos/0,
         base_url/1, domains/1, static_prefix/1, 
         translator_pid/1, router_config/1, application_info/1,
         reload_translation/2, reload_all_translations/1, reload_routes/1, reload_init_scripts/1,
         get_all_routes/1, get_all_models/1, get_all_application_names/1,
         update_info/1]).

-include("boss_web.hrl").

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

%% @spec get_all_application_infos() -> [#boss_app_info{}]
%% @doc Returns the full set of application infos.
get_all_application_infos() ->
    lists:map(fun({_Application, Info}) ->
                      Info
              end, ets:tab2list(?BOSS_APPLICATION_TABLE)).

%% @spec set_application_infos([#boss_app_info{}]) -> ok
%% @doc Store application info using mochiglobal.
set_application_infos(AppInfos) ->
    lists:foreach(fun(#boss_app_info{ application=Application }=AppInfo) ->
                          ets:insert(?BOSS_APPLICATION_TABLE, {Application, AppInfo})
                  end, AppInfos).

%% Calls that require access to web controller state.

%% @spec reload_routes() -> [ok]
%% @doc Reloads all routes known to the web controller.
reload_routes() ->
    reload_routes(get_all_application_infos()).

%% @spec reload_translation(string()) -> ok
%% @doc Reloads the translation for the specified locale.
reload_translation(Locale) ->
    reload_translation(get_all_application_infos(), Locale).

%% @spec reload_all_translations() -> [ok]
%% @doc Reloads all translations known to the web controller.
reload_all_translations() ->
    reload_all_translations(get_all_application_infos()).

%% @spec reload_init_scripts() -> [#boss_app_info{}]
%% @doc Runs all init scripts and returns a list of
%% application infos with init_data updated to the results
%% of the runs.
reload_init_scripts() ->
    reload_init_scripts(get_all_application_infos()).

%% @spec get_all_routes() -> list()
%% @doc Returns all routes known to the web controller.
get_all_routes() ->
    get_all_routes(get_all_application_infos()).

%% @spec get_all_models() -> list()
%% @doc Returns all models known to the web controller.
get_all_models() ->
    get_all_models(get_all_application_infos()).

%% @spec get_all_applications() -> [string()]
%% @doc Returns the names of all applications known to the web controller.
get_all_applications() ->
    get_all_application_names(get_all_application_infos()).

%% @spec base_url(App::string()) -> string() | ""
%% @doc Returns the base URL for the named application.
base_url(App) ->
    case application_info(App) of
        #boss_app_info{ base_url = BaseURL } ->
            BaseURL;
        _ ->
            ""
    end.

%% @spec domains(App::string()) -> [string()] | all
%% @doc Returns the list of domains for the named application.
domains(App) ->
    case application_info(App) of
        #boss_app_info{ domains = Domains } ->
            Domains;
        _ ->
            ""
    end.

%% @spec static_prefix(App::string()) -> string() | "/static"
%% @doc Returns the static prefix for the named application.
static_prefix(App) ->
    case application_info(App) of
        #boss_app_info{ static_prefix = StaticPrefix } ->
            StaticPrefix;
        _ ->
            ""
    end.

%% @spec translator_pid(App::string()) -> pid() | undefined
%% @doc Returns the translator pid for the named application.
translator_pid(App) ->
    case application_info(App) of
        #boss_app_info{ translator_sup_pid = SupPid } ->
            [{_, TranslatorPid, _, _}] = supervisor:which_children(SupPid),
            TranslatorPid;
        _ ->
            ""
    end.

%% @spec router_config(App::string()) -> #boss_router_config{} | undefined
%% @doc Returns the router configuration for the named application.
router_config(App) ->
    case application_info(App) of
        #boss_app_info{ router_config = RouterConfig } ->
            RouterConfig;
        _ ->
            ""
    end.

%% @spec application_info(App::string()) -> #boss_app_info{} | false
%% @doc Returns the complete application info for the named application.
application_info(App) ->
    case ets:lookup(?BOSS_APPLICATION_TABLE, App) of
        {ok, AppInfo} ->
            AppInfo;
        _ ->
            false
    end.

update_info(#boss_app_info{ application=Application }=AppInfo) ->
    ets:insert(?BOSS_APPLICATION_TABLE, {Application, AppInfo}).

%% Calls that depend only on their input arguments.

reload_translation(AppInfos, Locale) ->
    lists:map(fun(AppInfo) ->
                      [{_, TranslatorPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
                      boss_translator:reload(TranslatorPid, Locale)
              end, AppInfos).

reload_all_translations(AppInfos) ->
    lists:map(fun(AppInfo) ->
                      [{_, TranslatorPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
                      boss_translator:reload_all(TranslatorPid)
              end, AppInfos).

reload_routes(AppInfos) ->
    lists:map(fun(_AppInfo) ->
                      boss_router:reload()
              end, AppInfos).

%% Reloading init scripts changes the stored application state.
reload_init_scripts(AppInfos) ->
    NewAppInfos = lists:map(fun(AppInfo) ->
                                    boss_web_controller:stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data),
                                    NewInitData = bos_web_controller:run_init_scripts(AppInfo#boss_app_info.application),
                                    AppInfo#boss_app_info{ init_data = NewInitData }
                            end, AppInfos),
    set_application_infos(NewAppInfos),
    NewAppInfos.

get_all_routes(AppInfos) ->
    lists:map(fun(AppInfo) ->
                      {AppInfo#boss_app_info.application, boss_router:get_all()}
              end, AppInfos).

get_all_models(AppInfos) ->
    lists:foldl(fun(AppInfo, Acc) ->
                        boss_files:model_list(AppInfo#boss_app_info.application) ++ Acc
                end, [], AppInfos).

get_all_application_names(AppInfos) ->
    lists:map(fun(AppInfo) -> AppInfo#boss_app_info.application end, AppInfos).

%% get_translator_pid(AppInfos, App) ->
%%     lists:foldl(fun
%%                     (#boss_app_info{ application = App1 }=AppInfo, _) when App1 =:= App ->
%%                         get_translator_pid(AppInfo);
%%                     (_, Res) ->
%%                         Res
%%                 end, undefined, AppInfos).

%% get_translator_pid(#boss_app_info{ translator_sup_pid = SupPid }) ->
%%     [{_, TranslatorPid, _, _}] = supervisor:which_children(SupPid),
%%     TranslatorPid.

%% get_router_config(AppInfos, App) ->
%%     lists:foldl(fun
%%                     (#boss_app_info{ application = App1, router_config = Config }, _) when App1 =:= App ->
%%                         Config;
%%                     (_, Res) ->
%%                         Res
%%                 end, undefined, AppInfos).

%% %% Filter out unwanted fields
%% get_base_url(AppInfos, App) ->
%%     lists:foldl(fun
%%                     (#boss_app_info{ application = App1, base_url = URL }, _) when App1 =:= App ->
%%                         URL;
%%                     (_, Res) ->
%%                         Res
%%                 end, "", AppInfos).

%% get_static_prefix(AppInfos, App) ->
%%     lists:foldl(fun
%%                     (#boss_app_info{ application = App1, static_prefix = Prefix }, _) when App1 =:= App ->
%%                         Prefix;
%%                     (_, Res) ->
%%                         Res
%%                 end, "/static", AppInfos).

%% get_domains(AppInfos, App) ->
%%     lists:foldl(fun
%%                     (#boss_app_info{ application = App1, domains = Domains}, _) when App1 =:= App ->
%%                         Domains;
%%                     (_, Res) ->
%%                         Res
%%                 end, all, AppInfos).
