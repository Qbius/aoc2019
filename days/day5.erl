-module(day5).
-export([fst/1, snd/1]).

fst(Input) ->
    Tape = intcode:parse(Input),
    {_, Outputs} = intcode:run(Tape, [1]),
    lists:last(Outputs).

snd(Input) ->
    Tape = intcode:parse(Input),
    {_, Outputs} = intcode:run(Tape, [5]),
    lists:last(Outputs).