-module(ebot_proc).
-include("ebot_a.hrl").
-include("ebot_persist.hrl").

-export([in/1]).

in(P) -> 
	F = ?FROM(P),
	L = [
		{?G, fun() -> guru(P) end}, 
		{?C, fun() -> conf(P) end}],
	Process = lists:foldl(fun({J, CPr}, Pr) -> 
				JID = ?JID(J),
				case ?JIDCMP(F, JID) of
					true -> CPr;
					false -> Pr
				end
			end, 
		fun() -> ?AS(ebot_a:send(?G, ?L(?FROMP(P)) ++ ": " ++ ?L(?BODY(P)))) end, 
		L),
	Process(). 


%
% Guru
%
guru(P) ->
	case ?BODY(P) of
		undefined -> ok;
		M ->
			case M of
				<<"S">> -> 
					?AS(ebot_a:join(?C, ?CN));
				<<"U">> -> 
					?AS(ebot_a:leave(?C, ?CN));
				_ -> 
					?AS(ebot_a:send(?G, "Не понял, хозяин?"))
			end
	end.

%
% Conference
%
conf(P) ->
	LNick = conf_nick(P),
	% check body
	case ?BODY(P) of
		undefined -> ok;
		Me ->
			% send message to guru
			?AS(ebot_a:send(?G, LNick ++ ": " ++ ?L(Me))),
			% its not me?
			case LNick == ?CN of 
				true -> ok;
				_ -> 
					% its to me
					case re:run(Me, "^[ ]*" ++ ?CN, [{capture,[1],list}]) of
						{match, _} -> conf_process(LNick, Me);
						_ -> conf_watch(LNick, Me)
					end
			end		
	end.

conf_nick(P) ->
	{_, _, Nick} = ?FROMM(P),
	case Nick of
		undefined -> "[System]";
		N -> ?L(N)
	end.

conf_process(LNick, Me) ->
	PA = [
			{ "(сиськи|сиски|соски|cbcrb|cbcmrb|cjcrb){1}( для | for | дле | за | для )?(.*)", 
			  	fun(M) -> 
					B = find_boobs(),
					case M of
						[_, _, _, Nick] -> ?AS(ebot_a:say(?C, ?CN, Nick ++ B));
						_ -> ?AS(ebot_a:say(?C, ?CN, LNick ++ B))
					end
			  	end },
			{ "че там в ([^?.]*)",
			  	fun([_,T]) -> 
					?AS(ebot_a:say(?C, ?CN, LNick ++ ", да чета нет в " ++ T ++ " ничего"))
			  	end }
		],
	PAR = list:foldl(fun({Re, F}, OF) ->
				case re:run(Me, Re, [{capture, all, list}]) of
					{match, C} -> fun() -> F(C) end; 
					_ -> OF
				end
			end, 
			fun() -> 
				Replies = ebot_persist:default(), 
				R = ebot_utils:rand_list(Replies),
				?AS(ebot_a:say(?C, ?CN, io_lib:format(R, [LNick])))
			end,
			PA),
	PAR().  


conf_watch(LNick, Me) -> ok.

find_boobs() -> 
	try 
		R = crypto:rand_uniform(1, 550),
		N = list_to_binary(io_lib:format("~p", [R])),
		L = parse_kmb(N),
		H = ebot_utils:rand_list(L),
		", на - " ++ H
	catch 
		_ -> 
			", пока не завезли."
	end.

parse_kmb(Num) ->
	{ok, _, _, C} = ibrowse:send_req("http://kissmebaby.ru/beauty-" ++ Num, [], get),
	case re:run(C, "(http://kissmebaby.ru/images/[0-9]{4}/.*_[0-9]{1,3}.jpg)", [global, {capture, [1], list}]) of
		{match, I} -> I;  
		_ -> []
	end.

