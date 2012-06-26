-module(ebot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_, _) ->
	ebot_sup:start_link().

stop(_State) ->
    ok.

