-module(day5).
-export([fst/0, snd/0]).

fst() ->
    {ok, Data} = file:read_file("day5.input"),
    Tape = list_to_tuple(lists:map(fun binary_to_integer/1, string:split(Data, ",", all))),
    lists:last(run(0, Tape, 1)).

snd() ->
    {ok, Data} = file:read_file("day5.input"),
    Tape = list_to_tuple(lists:map(fun binary_to_integer/1, string:split(Data, ",", all))),
    lists:last(run(0, Tape, 5)).

run(I, Tape, Input) ->
    {Operator, ParameterModes} = case lists:reverse(integer_to_list(get(I, Tape))) of
        [OpDigit2, OpDigit1 | ParamModes] -> {list_to_integer([OpDigit1, OpDigit2]), ParamModes};
        [SingleDigit] -> {SingleDigit - $0, []}
    end,
    GetParams = fun({N, true}) -> get_params(N, I, Tape, ParameterModes, true); (N) -> get_params(N, I, Tape, ParameterModes, false) end,
    Run = fun(Pointer, Turing) -> run(Pointer, Turing, Input) end,
    case Operator of
        99 -> 
            [];
        1 ->
            {Arg1, Arg2, Out} = GetParams(3),
            Run(I + 4, set(Out, Tape, Arg1 + Arg2));
        2 ->
            {Arg1, Arg2, Out} = GetParams(3),
            Run(I + 4, set(Out, Tape, Arg1 * Arg2));
        3 ->
            Run(I + 2, set(get(I + 1, Tape), Tape, Input));
        4 ->
            {Out} = GetParams(1),
            [get(Out, Tape) | Run(I + 2, Tape)];
        5 ->
            {Arg, Offset} = GetParams({2, true}),
            Run(case Arg of 0 -> I + 3; _ -> Offset end, Tape);
        6 ->
            {Arg, Offset} = GetParams({2, true}),
            Run(case Arg of 0 -> Offset; _ -> I + 3 end, Tape);
        7 ->
            {Arg1, Arg2, Out} = GetParams(3),
            Run(I + 4, set(Out, Tape, case Arg1 < Arg2 of true -> 1; false -> 0 end));
        8 ->
            {Arg1, Arg2, Out} = GetParams(3),
            Run(I + 4, set(Out, Tape, case Arg1 =:= Arg2 of true -> 1; false -> 0 end))
    end.

get(I, Tuple) -> 
    element(I + 1, Tuple).
set(I, Tuple, New) -> 
    setelement(I + 1, Tuple, New).

get_params(N, InitialI, Tape, ParameterModes, IgnoreWriting) ->
    FullParameterModes = fill_string_right(ParameterModes, $0, N),
    Args = list_to_tuple([case Mode of $0 -> get(get(I, Tape), Tape); $1 -> get(I, Tape) end || {Mode, I} <- lists:zip(FullParameterModes, lists:seq(InitialI + 1, InitialI + N))]),
    case IgnoreWriting of true -> Args; false -> setelement(N, Args, get(InitialI + N, Tape)) end.

fill_string_right(Str, _, N) when length(Str) =:= N -> 
    Str;
fill_string_right(Str, Char, N) -> 
    fill_string_right(Str ++ [Char], Char, N).