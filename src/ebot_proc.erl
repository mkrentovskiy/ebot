-module(ebot_proc).

-include("ebot_a.hrl").

-export([in/1]).

in(P) -> 
	F = ?FROM(P),
	JID = ?JID(?G), 
	case ?JIDCMP(F, JID) of 
		true ->
			case ?BODY(P) of
				undefined -> 
					ok;
				M ->
					process(M)
			end;
		false ->
			ok
	end.

process(<<"list">>) -> 
	Res = watcher:list(),
	?AS(ebot_a:send(?G, Res));
process(<<"add ", Url/binary>>) -> 
	UrlS = binary_to_list(Url),
	Res = watcher:add(page, UrlS),
	?AS(ebot_a:send(?G, Res));
process(<<"addrss ", Url/binary>>) -> 
	UrlS = binary_to_list(Url),
	Res = watcher:add(rss, UrlS),
	?AS(ebot_a:send(?G, Res));
process(<<"del ", Id/binary>>) -> 
	IdN = list_to_integer(binary_to_list(Id)),
	Res = watcher:del(IdN),
	?AS(ebot_a:send(?G, Res));
process(<<"run">>) -> 
	Res = watcher:run(),
	?AS(ebot_a:send(?G, Res));
process(<<"stop">>) -> 
	Res = watcher:stop(),
	?AS(ebot_a:send(?G, Res));
process(<<"now">>) -> 
	Res = watcher:now(),
	?AS(ebot_a:send(?G, Res));
process(_) ->
	?AS(ebot_a:send(?G, "[list|add _url_|addrss _url_|del _id_|run|stop|now]")).