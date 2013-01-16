-module(boss_application_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        applications = []
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Applications = proplists:get_value(applications, Args, []),
    {ok, #state{applications = Applications}}.

handle_call(reload_routes, _From, State) ->
    Result = boss_application:reload_routes(State#state.applications),
    {reply, Result, State};

handle_call({reload_translation, Locale}, _From, State) ->
    Result = boss_application:reload_translation(State#state.applications, Locale),
    {reply, Result, State};

handle_call(reload_all_translations, _From, State) ->
    Result = boss_application:reload_all_translations(State#state.applications),
    {reply, Result, State};

handle_call(reload_init_scripts, _From, State) ->
    Result = boss_application:reload_init_scripts(State#state.applications),
    {reply, Result, State};

handle_call(get_all_routes, _From, State) ->
    Routes = boss_application:get_all_routes(State#state.applications),
    {reply, Routes, State};

handle_call(get_all_models, _From, State) ->
    Models = boss_application:get_all_models(State#state.applications),
    {reply, Models, State};

handle_call(get_all_applications, _From, State) ->
    Applications = boss_application:get_all_application_names(State#state.applications),
    {reply, Applications, State};

handle_call({translator_pid, App}, _From, State) ->
    Pid = boss_application:get_translator_pid(State#state.applications, App),
    {reply, Pid, State};

handle_call({router_pid, App}, _From, State) ->
    Pid = boss_application:get_router_pid(State#state.applications, App),
    {reply, Pid, State};

handle_call(get_all_application_infos, _From, State) ->
    AppInfos = boss_application:get_all_application_infos(State#state.applications),
    {reply, AppInfos, State};

handle_call({application_info, App}, _From, State) ->
    AppInfo = lists:keyfind(App, 2, State#state.applications),
    {reply, AppInfo, State};

handle_call({base_url, App}, _From, State) ->
    BaseURL = boss_application:get_base_url(State#state.applications, App),
    {reply, BaseURL, State};

handle_call({static_prefix, App}, _From, State) ->
    StaticPrefix = boss_application:get_static_prefix(State#state.applications, App),
    {reply, StaticPrefix, State};

handle_call({domains, App}, _From, State) ->
    DomainList = boss_application:get_domains(State#state.applications, App),
    {reply, DomainList, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

