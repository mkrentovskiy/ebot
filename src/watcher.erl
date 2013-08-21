-module(watcher).
-behaivour(gen_server).

-include("ebot_a.hrl").

-export([list/0, add/2, del/1]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> 
	S = persist:get(),
	{ok, S}.

list() -> 
	gen_server:call(?MODULE, list).
add(Type, Url) -> 
	gen_server:call(?MODULE, {add, Type, Url}).
del(Id) -> 
	gen_server:call(?MODULE, {del, Id}).


handle_call(list, _From, S) -> 
	Res = lists:foldl(fun(I, A) -> 
			A ++ io_lib:format("~p : ~p : ~p~n", [I#src.id, I#src.type, I#src.url])
		end, [], S),
	CRes = case Res of
		[] -> "none";
		A -> A
	end,
    {reply, CRes, S};
handle_call({add, Type, Url}, _From, S) -> 
	Id = lists:foldl(fun(I, A) ->  
			case I#src.id > A of
				true ->
					I#src.id;
				false ->
					A
			end
		end, 0, S) + 1,
	NewS = S ++ [#src{ id = Id, url = Url, type = Type, state = []}],
	persist:update(NewS),
    {reply, "ok", NewS};
handle_call({del, Id}, _From, S) -> 
	NewS = lists:filter(fun(I) -> I#src.id =/= Id end, S),
    persist:update(NewS),
    {reply, ok, NewS};

handle_call(_Msg, _From, S) -> 
    {reply, ok, S}.
handle_cast(_Msg, S) -> 
    {noreply, S}.

handle_info(check, S) -> 
	NewS = check(S),
	next(),
    {noreply, NewS};   
handle_info(Msg, S) -> 
    {noreply, S}.

terminate(_Reason, S) -> ok.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%
% local
%

check([]) -> 
	[];
check(L) ->
	ebot_a:set_status("Working!"),
	R = lists:map(fun(I) ->
			State = check_src(I),
			I#src{ state = State }
		end, L),
	ebot_a:set_status("Waiting..."),
	R.

check_src(Src) ->
	Src.

next() ->
	{ok, _} = timer:send_after(?TIMEOUT, check).

% ebot_a:send(?G, Event).

get_and_parse(Url, RE) ->
	{ok, _, _, C} = ibrowse:send_req(Url, [], get),
	case re:run(C, RE, [global, {capture, all, list}]) of
		{match, I} -> {I, C};  
		_ -> []
	end.
