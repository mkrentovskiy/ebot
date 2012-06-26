-module(ebot).
-export([start/0, stop/0, env/2]).

start() ->
    ensure_started(crypto),
	ensure_started(exmpp),
    ensure_started(mnesia),
    ensure_started(ibrowse),
    application:start(ebot).

stop() ->
    application:stop(mnesia),    
    application:stop(ibrowse),    
	application:stop(exmpp),	
	application:stop(ebot).

env(Param, Default) ->
        case application:get_env(?MODULE, Param) of
                {ok, Value} -> Value;
                _ -> Default
        end.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
