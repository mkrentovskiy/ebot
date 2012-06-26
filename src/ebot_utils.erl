-module(ebot_utils).
-export([rand_list/1]).

rand_list(L) -> RL = length(L), lists:nth(L, crypto:rand_uniform(1, RL)).
