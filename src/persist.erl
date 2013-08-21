-module(persist).

-include("ebot_a.hrl").

-export([init_db/0, update/1, get/0]).

init_db() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(src, 
		[{disc_copies, [node()]}, 
		{attributes, record_info(fields, src)}]).


update(L) ->
	ok.

get() ->
	[].