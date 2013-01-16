%% @doc Manage application state for the web controller process.

-module(boss_application).

-export([start/0, start/1, stop/0]).
-export([reload_routes/0, reload_translation/1, reload_all_translations/0,
         reload_init_scripts/0, get_all_routes/0, get_all_models/0, get_all_applications/0,
         get_all_application_infos/0, base_url/1, domains/1, static_prefix/1, 
         translator_pid/1, router_pid/1, application_info/1,
         reload_translation/2, reload_all_translations/1, reload_routes/1, reload_init_scripts/1,
         get_all_routes/1, get_all_models/1, get_all_application_names/1,
         get_translator_pid/2, get_router_pid/2,
         get_all_application_infos/1, get_base_url/2, get_static_prefix/2, get_domains/2]).

-define(POOLNAME, boss_application_pool).

-include("boss_web.hrl").

start() ->
    start([]).

start(AppInfos) ->
    boss_application_sup:start_link(AppInfos).

stop() ->
    ok.

%% Calls that require access to web controller state and are performed on
%% poolboy workers.

%% @spec reload_routes() -> [ok]
%% @doc Reloads all routes known to the web controller.
reload_routes() ->
    boss_pool:call(?POOLNAME, reload_routes).

%% @spec reload_translation(string()) -> ok
%% @doc Reloads the translation for the specified locale.
reload_translation(Locale) ->
    boss_pool:call(?POOLNAME, {reload_translation, Locale}).

%% @spec reload_all_translations() -> [ok]
%% @doc Reloads all translations known to the web controller.
reload_all_translations() ->
    boss_pool:call(?POOLNAME, reload_all_translations).

%% @spec reload_init_scripts() -> [#boss_app_info{}]
%% @doc Runs all init scripts and returns a list of
%% application infos with init_data updated to the results
%% of the runs.
reload_init_scripts() ->
    boss_pool:call(?POOLNAME, reload_init_scripts).

%% @spec get_all_routes() -> list()
%% @doc Returns all routes known to the web controller.
get_all_routes() ->
    boss_pool:call(?POOLNAME, get_all_routes).

%% @spec get_all_models() -> list()
%% @doc Returns all models known to the web controller.
get_all_models() ->
    boss_pool:call(?POOLNAME, get_all_models).

%% @spec get_all_applications() -> [string()]
%% @doc Returns the names of all applications known to the web controller.
get_all_applications() ->
    boss_pool:call(?POOLNAME, get_all_applications).

%% @spec get_all_application_infos() -> [#boss_app_info{}]
%% @doc Returns the full set of application infos from the web controller.
get_all_application_infos() ->
    boss_pool:call(?POOLNAME, get_all_application_infos).

%% @spec base_url(App::string()) -> string() | ""
%% @doc Returns the base URL for the named application.
base_url(App) ->
    boss_pool:call(?POOLNAME, {base_url, App}).

%% @spec domains(App::string()) -> [string()] | all
%% @doc Returns the list of domains for the named application.
domains(App) ->
    boss_pool:call(?POOLNAME, {domains, App}).

%% @spec static_prefix(App::string()) -> string() | "/static"
%% @doc Returns the static prefix for the named application.
static_prefix(App) ->
    boss_pool:call(?POOLNAME, {static_prefix, App}).

%% @spec translator_pid(App::string()) -> pid() | undefined
%% @doc Returns the translator pid for the named application.
translator_pid(App) ->
    boss_pool:call(?POOLNAME, {translator_pid, App}).

%% @spec router_pid(App::string()) -> pid() | undefined
%% @doc Returns the router pid for the named application.
router_pid(App) ->
    boss_pool:call(?POOLNAME, {router_pid, App}).

%% @spec application_info(App::string()) -> #boss_app_info{} | false
%% @doc Returns the complete application info for the named application.
application_info(App) ->
    boss_pool:call(?POOLNAME, {application_info, App}).


%% Calls that depend only on their input arguments and are performed
%% within the calling process itself.

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
    lists:map(fun(AppInfo) ->
                      [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                      boss_router:reload(RouterPid)
              end, AppInfos).

reload_init_scripts(AppInfos) ->
    lists:map(fun(AppInfo) ->
                      boss_web_controller:stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data),
                      NewInitData = bos_web_controller:run_init_scripts(AppInfo#boss_app_info.application),
                      AppInfo#boss_app_info{ init_data = NewInitData }
              end, AppInfos).

get_all_routes(AppInfos) ->
    lists:map(fun(AppInfo) ->
                      [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                      {AppInfo#boss_app_info.application, boss_router:get_all(RouterPid)}
              end, AppInfos).

get_all_models(AppInfos) ->
    lists:foldl(fun(AppInfo, Acc) ->
                        boss_files:model_list(AppInfo#boss_app_info.application) ++ Acc
                end, [], AppInfos).

get_all_application_names(AppInfos) ->
    lists:map(fun(AppInfo) -> AppInfo#boss_app_info.application end, AppInfos).

get_translator_pid(AppInfos, App) ->
    lists:foldl(fun
                    (#boss_app_info{ application = App1 }=AppInfo, _) when App1 =:= App ->
                        get_translator_pid(AppInfo);
                    (_, Res) ->
                        Res
                end, undefined, AppInfos).

get_translator_pid(#boss_app_info{ translator_sup_pid = SupPid }) ->
    [{_, TranslatorPid, _, _}] = supervisor:which_children(SupPid),
    TranslatorPid.

get_router_pid(AppInfos, App) ->
    lists:foldl(fun
                    (#boss_app_info{ application = App1 }=AppInfo, _) when App1 =:= App ->
                        get_router_pid(AppInfo);
                    (_, Res) ->
                        Res
                end, undefined, AppInfos).

get_router_pid(#boss_app_info{ router_sup_pid = SupPid }) ->
    [{_, RouterPid, _, _}] = supervisor:which_children(SupPid),
    RouterPid.

%% Filter out unwanted fields
get_all_application_infos(AppInfos) ->
    lists:map(fun(AppInfo) -> AppInfo#boss_app_info{ init_data = undefined } end, AppInfos).

get_base_url(AppInfos, App) ->
    lists:foldl(fun
                    (#boss_app_info{ application = App1, base_url = URL }, _) when App1 =:= App ->
                        URL;
                    (_, Res) ->
                        Res
                end, "", AppInfos).

get_static_prefix(AppInfos, App) ->
    lists:foldl(fun
                    (#boss_app_info{ application = App1, static_prefix = Prefix }, _) when App1 =:= App ->
                        Prefix;
                    (_, Res) ->
                        Res
                end, "/static", AppInfos).

get_domains(AppInfos, App) ->
    lists:foldl(fun
                    (#boss_app_info{ application = App1, domains = Domains}, _) when App1 =:= App ->
                        Domains;
                    (_, Res) ->
                        Res
                end, all, AppInfos).
