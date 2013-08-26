-module(watcher).
-behaivour(gen_server).

-include("ebot_a.hrl").

-export([list/0, add/2, del/1, run/0, stop/0, now/0]).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> 
	S = persist:get(),
	next(),
	{ok, S}.

list() -> 
	gen_server:call(?MODULE, list).
add(Type, Url) -> 
	gen_server:call(?MODULE, {add, Type, Url}).
del(Id) -> 
	gen_server:call(?MODULE, {del, Id}).
run() -> 
	gen_server:call(?MODULE, run).
stop() -> 
	gen_server:call(?MODULE, stop).
now() -> 
	gen_server:call(?MODULE, now).
set(Src) ->
	gen_server:call(?MODULE, {set, Src}).


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
	NewS = case lists:filter(fun(I) -> I#src.id == Id end, S) of
		[Src] ->
			persist:del(Src#src.url),		
			lists:filter(fun(I) -> I#src.id =/= Id end, S);
		_ -> S
	end,
    {reply, ok, NewS};

handle_call(run, _From, S) -> 
	next(),
	{reply, "ok", S};

handle_call(stop, _From, S) -> 
	{reply, "no", S};

handle_call(now, _From, S) -> 
	check(S),
	{reply, "done", S};

handle_call({set, Src}, _From, S) -> 
	NewS = lists:map(fun(I) ->
			case I#src.url == Src#src.url of 
				true -> Src;
				false -> I
			end
		end, S),
	{reply, "done", NewS};

handle_call(_Msg, _From, S) -> 
    {reply, ok, S}.

handle_cast(_Msg, S) -> 
    {noreply, S}.

handle_info(check, S) -> 
	check(S),
	next(),
    {noreply, S};   
handle_info(Msg, S) -> 
    {noreply, S}.

terminate(_Reason, S) -> ok.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%
% local
%

check([]) -> [];
check(L)  -> lists:map(fun(I) -> ?AS(check_src(I)) end, L).

check_src(Src) ->
	?AS(ebot_a:set_status("Check " ++ Src#src.url)),
	NS = case get_and_parse(Src#src.url, Src#src.type, "<title>(.+)</title>") of
		{ok, L} ->
			lists:map(fun(I) ->
					SStr = case Src#src.type of
						page -> I;		
						rss ->re:replace(
								re:replace(I, "\\]\\]>", "", [{return, binary}]), 
								"<!\\[CDATA\\[", "", [{return, binary}])
					end,
					Str = re:replace(SStr, "</?[^<]*>", "", [global, {return, binary}]),
					case lists:filter(fun(J) -> J == Str end, Src#src.state) of
						[] ->
							Url = list_to_binary("\n" ++ Src#src.url),
							Msg = <<Str/binary, Url/binary>>,
							%% io:format(">>>~n~ts~n", [unicode:characters_to_list(Msg)]),
							ebot_a:send(?G, Msg),
							Str;
						_ ->
							Str					
					end
				end, L);
		error ->
			ebot_a:send(?G, "Error: " ++ Src#src.url),
			Src#src.state
	end, 
	?AS(ebot_a:set_status("Done")),
	set(Src#src{ state = NS }).

next() ->
	{ok, _} = timer:send_after(?TIMEOUT, check).

get_and_parse(Url, Type, RE) ->
	RH = [
		{ "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" },
		{ "Accept-Language", "ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4" },
		{ "User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/28.0.1500.71 Chrome/28.0.1500.71 Safari/537.36" }
	],
	case ibrowse:send_req(Url, RH, get) of
		{ok, _, H, CS} ->
			C = decode(CS, Type, H),
			case re:run(C, RE, [global, {capture, all, binary}]) of
				{match, I} -> {ok, I};
				_ -> error
			end;
		_ ->
			error
	end.

decode(C, rss, _) ->
	case re:run(C, "encoding=\"([a-zA-Z0-9\\-]+)\"", [{capture, all, list}]) of
		{match, [_, Enc]} -> iconv:convert(Enc, "UTF8", C);
		_ -> C
	end;
decode(C, page, H) ->
	case lists:filter(fun({IH, _}) -> IH == "Content-Type" end, H) of
		[{_, V}] -> 
			case re:run(V, "charset=([a-zA-Z0-9\\-]+)", [{capture, all, list}]) of
				{match, [_, Enc]} -> iconv:convert(Enc, "UTF8", C);
				_ -> C
			end;
		_ ->
			C
	end.


