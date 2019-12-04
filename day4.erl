-module(day4).
-export([fst/0, snd/0]).

fst() ->
    {ok, Data} = file:read_file("day4.input"),
    [LowerBound, UpperBound] = lists:map(fun list_to_integer/1, string:split(string:trim(binary_to_list(Data)), "-", all)),
    AllPass = [list_to_integer([A, B, C, D, E, F]) || A <- lists:seq($1, $9), B <- lists:seq(A, $9), C <- lists:seq(B, $9), D <- lists:seq(C, $9), E <- lists:seq(D, $9), F <- lists:seq(E, $9), A =:= B orelse B =:= C orelse C =:= D orelse D =:= E orelse E =:= F],
    length(lists:filter(fun(N) -> N >= LowerBound andalso N =< UpperBound end, AllPass)).

snd() ->
    {ok, Data} = file:read_file("day4.input"),
    [LowerBound, UpperBound] = lists:map(fun list_to_integer/1, string:split(string:trim(binary_to_list(Data)), "-", all)),
    AllPass = [
        list_to_integer([A, B, C, D, E, F]) || A <- lists:seq($1, $9), B <- lists:seq(A, $9), C <- lists:seq(B, $9), D <- lists:seq(C, $9), E <- lists:seq(D, $9), F <- lists:seq(E, $9), 
        (A =:= B andalso B =/= C) orelse 
        (A =/= B andalso B =:= C andalso C =/= D) orelse 
        (B =/= C andalso C =:= D andalso D =/= E) orelse 
        (C =/= D andalso D =:= E andalso E =/= F) orelse 
        (D =/= E andalso E =:= F)
    ],
    length(lists:filter(fun(N) -> N >= LowerBound andalso N =< UpperBound end, AllPass)).