
-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_REVERSE_ROUTES_TABLE, boss_reverse_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).

-record(boss_route, {number, url, pattern, application, controller, action, params = []}).

-record(boss_reverse_route, {application_controller_action_params, url}).

-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(state, {
        application,
        controllers = [],
        routes_table_id,
        reverse_routes_table_id,
        handlers_table_id
    }).
