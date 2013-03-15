-module(boss_session_adapter_mock).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1, init/1]).
-export([session_exists/2, create_session/3, lookup_session/2]).
-export([lookup_session_value/3, set_session_value/4, delete_session/2, delete_session_value/3]).

%% Maintain the mock session adapter state in a singleton ETS table entry
-define(BOSS_SESSION_ADAPTER_MOCK_TABLE, boss_session_adapter_mock_state).
-record(boss_session, {sid, data}).

-record(state, {
        table, 
        session_dict = dict:new(),
        ttl_tree = gb_trees:empty(),
        exp_time
    }).


start() ->
    start([]).

start(Options) ->
    setup_table(),
    TableOfSessions = ets:new(?MODULE,[set,public,named_table,{keypos, 2}]),
    State = #state{ table = TableOfSessions,
                    exp_time = proplists:get_value(session_exp_time, Options, 1440) },
    set_state(State),
    {ok, undefined}.

stop(undefined) ->
    teardown_table(),
    ok.

init(_Options) ->
    ok.

setup_table() ->
    case ets:info(?BOSS_SESSION_ADAPTER_MOCK_TABLE) of
        undefined -> ets:new(?BOSS_SESSION_ADAPTER_MOCK_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _X -> ets:delete_all_objects(?BOSS_SESSION_ADAPTER_MOCK_TABLE)
    end.

teardown_table() ->
    case ets:info(?BOSS_SESSION_ADAPTER_MOCK_TABLE) of
        undefined -> ok;
        _X ->
            ets:delete(?BOSS_SESSION_ADAPTER_MOCK_TABLE)
    end.

get_state() ->
    [{state, State}] = ets:lookup(?BOSS_SESSION_ADAPTER_MOCK_TABLE, state),
    State.

set_state(State) ->
    ets:insert(?BOSS_SESSION_ADAPTER_MOCK_TABLE, {state, State}).

session_exists(_, undefined) ->
    false;
session_exists(_, SessionID) ->
    State = get_state(),
    Exists = dict:is_key(SessionID, State#state.session_dict),
    NewState = case Exists of
        true ->
            NowSeconds = now_seconds(),
            Val = dict:fetch(SessionID, State#state.session_dict),
            State#state{ 
                ttl_tree = tiny_pq:move_value(Val, NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree),
                session_dict = dict:store(SessionID,
                    NowSeconds + State#state.exp_time,
                    State#state.session_dict)};
        false ->
            State
    end,
    set_state(prune_expired_sessions(NewState, now_seconds())),
    Exists.

create_session(_, SessionID, Data) ->
    State = get_state(),
    NowSeconds = now_seconds(),
    ets:insert(State#state.table, #boss_session{sid=SessionID, data=Data}),
    NewState = State#state{ 
        ttl_tree = tiny_pq:insert_value(NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree),
        session_dict = dict:store(SessionID, NowSeconds + State#state.exp_time, State#state.session_dict)
    },
    set_state(NewState),
    ok.

lookup_session(_, SessionID) ->
    State = get_state(),
    case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end.

lookup_session_value(_, SessionID, Key) ->
    State = get_state(),
    Data = case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end,
    proplists:get_value(Key, Data).

set_session_value(_, SessionID, Key, Value) ->
    State = get_state(),
    Data = case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end,
    Data1 = case proplists:is_defined(Key,Data) of
        true ->
            Rest = proplists:delete(Key,Data),
            [{Key,Value}|Rest];
        false ->
            [{Key,Value}|Data]
    end,
    ets:insert(State#state.table, #boss_session{sid=SessionID,data=Data1}),
    ok.

delete_session(_, SessionID) ->
    State = get_state(),
    ets:delete(State#state.table, SessionID),
    NewState = case dict:find(SessionID, State#state.session_dict) of
        {ok, Val} ->
            State#state{ 
                ttl_tree = tiny_pq:delete_value(Val, SessionID, State#state.ttl_tree),
                session_dict = dict:erase(SessionID, State#state.session_dict)
            };
        _ ->
            State
    end,
    set_state(NewState),
    ok.

delete_session_value(_, SessionID, Key) ->
    State = get_state(),
    case ets:lookup(?MODULE,SessionID) of
        [S] ->
            Data = S#boss_session.data,
            case proplists:is_defined(Key,Data) of
                true ->
                    Data1 = proplists:delete(Key,Data),
                    ets:insert(State#state.table, #boss_session{ sid=SessionID, data=Data1 });
                false ->
                    ok
            end;
        [] -> 
            ok
    end,
    ok.

prune_expired_sessions(#state{ ttl_tree = Tree, session_dict = Dict, table = TableId } = State, NowSeconds) ->
    {NewDict, NewTree} = tiny_pq:prune_collect_old(fun(SessionID, DictAcc) ->
                ets:delete(TableId, SessionID),
                dict:erase(SessionID, DictAcc)
        end, Dict, Tree, NowSeconds),
    State#state{ ttl_tree = NewTree, session_dict = NewDict }.

now_seconds() ->
    {A, B, _} = erlang:now(),
    A * 1000 * 1000 + B.
