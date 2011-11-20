-module(price).
-export([compute_map/1, compute_lc/1]).

compute_map(L) -> lists:map(fun({Item, Quantity, Price}) -> {Item, Quantity * Price} end, L).

compute_lc(L) -> [{Item, Quantity * Price} || {Item, Quantity, Price} <- L].