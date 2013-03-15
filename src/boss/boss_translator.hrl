
-define(BOSS_TRANSLATOR_TABLE, boss_translator).

%% The translation state consists of a dictionary of dictionaries, mapping locales
%% to strings to translated strings, and an application name.
-record(state, {strings,
                application}).
