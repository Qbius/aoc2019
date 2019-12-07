-module(day5).
-export([fst/1, snd/1]).

fst(FileInput) ->
    Tape = list_to_tuple(lists:map(fun binary_to_integer/1, string:split(FileInput, ",", all))),
    {_, Outputs} = intcode:run(Tape, [1]),
    lists:last(Outputs).

snd(FileInput) ->
    Tape = list_to_tuple(lists:map(fun binary_to_integer/1, string:split(FileInput, ",", all))),
    {_, Outputs} = intcode:run(Tape, [5]),
    lists:last(Outputs).