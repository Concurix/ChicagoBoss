%% @doc Chicago Boss translator service

-module(boss_translator).

-export([start/0, start/1, stop/0]).

-export([
        is_loaded/2,
        lookup/3, 
        fun_for/2,
        reload/2,
        reload_all/1
    ]).

-include("boss_translator.hrl").

start() ->
    start([]).

start(Options) ->
    BossApp = proplists:get_value(application, Options),
    StringDictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_strings(BossApp, Lang))}
        end, boss_files:language_list(BossApp)),
    State = #state{
               strings = dict:from_list(StringDictionaryList), 
               application = BossApp },
    {ok, State}.

stop() ->
    ok.

%% @spec lookup(Key::string(), Locale::string()) -> Translation::string() | undefined
lookup(State, Key, Locale) ->
    case dict:find(Locale, State#state.strings) of
        {ok, Dict} ->
            case dict:find(Key, Dict) of
                {ok, Trans} -> Trans;
                error -> undefined
            end;
        _ ->
            undefined
    end.

%% @spec is_loaded(Locale::string()) -> true | false
is_loaded(State, Locale) ->
     dict:is_key(Locale, State#state.strings).

%% @spec reload(Locale::string()) -> #state()
reload(State, Locale) ->
    StringDict = dict:from_list(boss_lang:extract_po_strings(State#state.application, Locale)),
    State#state{
        strings = dict:store(Locale, StringDict, State#state.strings)
    }.

%% @spec reload_all() -> #state()
reload_all(State) ->
    lists:foldr(fun(X, StateAcc) -> 
                        reload(StateAcc, X)
                end, State, boss_files:language_list(State#state.application)).

%% @spec fun_for(Locale::string()) -> TranslationFun::function() | none
fun_for(State, Locale) ->
    case is_loaded(State, Locale) of
        true -> fun(String) -> lookup(State, String, Locale) end;
        false -> none
    end.
