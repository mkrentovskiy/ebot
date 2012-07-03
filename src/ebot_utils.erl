-module(ebot_utils).
-export([rand_list/1]).

rand_list(L) when length(L) > 1 -> 
	RL = length(L), 
	lists:nth(crypto:rand_uniform(1, RL), L);
rand_list([P]) -> P;
rand_list([]) -> "". 
