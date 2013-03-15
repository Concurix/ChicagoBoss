%% @doc Chicago Boss session handler abstraction

-module(boss_session).

-export([start/0, start/1, stop/0]).
-export([get_session_key/0, get_session_exp_time/0]).
-export([new_session/1, get_session_data/1, get_session_data/2, set_session_data/3]).
-export([remove_session_data/2, delete_session/1]).

%% Maintain the adapter and connection state in a singleton ETS table entry
-define(BOSS_SESSION_TABLE, boss_session_state).
-record(state, {
          adapter, 
          connection
         }).

start() ->
    SessionOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [session_key, session_exp_time]),
    SessionDriver = boss_env:get_env(session_adapter, mock),
    Adapter = list_to_atom(lists:concat(["boss_session_adapter_", SessionDriver])),
    Adapter:init([]),
    SessionOptions1 = [{adapter, Adapter}|SessionOptions],
    start(SessionOptions1).

start(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_mock),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, Conn} = Adapter:start(Options),
    State = #state{adapter = Adapter, connection = Conn },
    setup_table(),
    set_state(State),
    {ok, State}.

stop() ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:stop(Conn),
    teardown_table(),
    ok.

setup_table() ->
    case ets:info(?BOSS_SESSION_TABLE) of
        undefined -> ets:new(?BOSS_SESSION_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _X -> ets:delete_all_objects(?BOSS_SESSION_TABLE)
    end.

teardown_table() ->
    case ets:info(?BOSS_SESSION_TABLE) of
        undefined -> ok;
        _X ->
            ets:delete(?BOSS_SESSION_TABLE)
    end.

get_state() ->
    [{state, State}] = ets:lookup(?BOSS_SESSION_TABLE, state),
    State.

set_state(State) ->
    ets:insert(?BOSS_SESSION_TABLE, {state, State}).

get_session_key() ->
    boss_env:get_env(session_key, "_boss_session").

get_session_exp_time() ->
    boss_env:get_env(session_exp_time, 1440).

%% @spec new_session(Cookie::string()) -> string | {error, Reason}
%% @doc Starts new session with the specified `Cookie'.
new_session(Cookie) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    NewSessionID = case Adapter:session_exists(Conn, Cookie) of
        true ->
            Cookie;
        false ->
            SessionID = generate_session_id(),
            Adapter:create_session(Conn, SessionID, []),
            SessionID
    end,
    NewSessionID.


%% @spec get_session_data(SessionID) -> list | {error, Reason}
%% @doc Get session data for the `SessionID'.
get_session_data(SessionID) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:lookup_session(Conn, SessionID).

%% @spec get_session_data(SessionID, Key) -> list | {error, Reason}
%% @doc Get session data for the `SessionID' for a given `Key'.
get_session_data(SessionID, Key) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:lookup_session_value(Conn, SessionID, Key).

%% @spec set_session_data(SessionID, Key, Value) -> ok | {error, Reason}
%% @doc Set session data for the `SessionID'.
set_session_data(SessionID, Key, Value) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:set_session_value(Conn, SessionID, Key, Value).

%% @spec delete_session(SessionID) -> ok | {error, Reason}
%% @doc Delete session for given `SessionID'.
delete_session(SessionID) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:delete_session(Conn, SessionID).

%% @spec remove_session_data(SessionID, Key) -> ok | {error, Reason}
%% @doc Remove the Key from session data for the `SessionID'.
remove_session_data(SessionID, Key) ->
    #state{ adapter=Adapter, connection=Conn } = get_state(),
    Adapter:delete_session_value(Conn, SessionID, Key).

generate_session_id() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    lists:flatten(list_to_hex(Sha_list)).
%% Convert Integer from the SHA to Hex
list_to_hex(L)->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 -> 
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
