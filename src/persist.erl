-module(persist).

-include("ebot_a.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init_db/0, update/1, get/0, del/1]).

init_db() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(src, 
		[{disc_copies, [node()]}, 
		{attributes, record_info(fields, src)}]).


update(L) ->
	mnesia:transaction(fun() -> lists:map(fun(I) -> mnesia:write(I) end, L) end).

del(Url) ->
	mnesia:transaction(fun() -> mnesia:delete({src, Url}) end).

get() ->
	case mnesia:transaction(fun() -> qlc:e(qlc:q([X || X <- mnesia:table(src)])) end) of
		{atomic, List} -> List;
		_ -> []
	end.