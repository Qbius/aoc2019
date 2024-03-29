-module(intcode).
-export([parse/1, run/1, run/2]).

parse(Data) ->
    list_to_tuple(lists:map(fun binary_to_integer/1, string:split(Data, ",", all))).

run(Tape) -> run(0, Tape, 0, [], []).
run(Tape, Inputs) -> run(0, Tape, 0, Inputs, []).
run(I, Tape, RelativeBase, Inputs, Outputs) ->
    {Operator, ParameterModes} = case lists:reverse(integer_to_list(get(I, Tape))) of
        [OpDigit2, OpDigit1 | ParamModes] -> {list_to_integer([OpDigit1, OpDigit2]), ParamModes};
        [SingleDigit] -> {SingleDigit - $0, []}
    end,
    GetParams = fun({N, true}) -> get_params(N, I, Tape, ParameterModes, RelativeBase, true); (N) -> get_params(N, I, Tape, ParameterModes, RelativeBase, false) end,
    case Operator of
        99 -> 
            {Tape, lists:reverse(Outputs)};
        1 ->
            {Arg1, Arg2, Out} = GetParams(3),
            run(I + 4, set(Out, Tape, Arg1 + Arg2), RelativeBase, Inputs, Outputs);
        2 ->
            {Arg1, Arg2, Out} = GetParams(3),
            run(I + 4, set(Out, Tape, Arg1 * Arg2), RelativeBase, Inputs, Outputs);
        3 ->
            case Inputs of
                [Input | RestInputs] ->
                    run(I + 2, set(get(I + 1, Tape), Tape, Input), RelativeBase, RestInputs, Outputs);
                [] ->
                    {waiting_for_input, Tape, lists:reverse(Outputs)}
            end;
        4 ->
            {Out} = GetParams(1),
            run(I + 2, Tape, RelativeBase, Inputs, [get(Out, Tape) | Outputs]);
        5 ->
            {Arg, Offset} = GetParams({2, true}),
            run(case Arg of 0 -> I + 3; _ -> Offset end, Tape, RelativeBase, Inputs, Outputs);
        6 ->
            {Arg, Offset} = GetParams({2, true}),
            run(case Arg of 0 -> Offset; _ -> I + 3 end, Tape, RelativeBase, Inputs, Outputs);
        7 ->
            {Arg1, Arg2, Out} = GetParams(3),
            run(I + 4, set(Out, Tape, case Arg1 < Arg2 of true -> 1; false -> 0 end), RelativeBase, Inputs, Outputs);
        8 ->
            {Arg1, Arg2, Out} = GetParams(3),
            run(I + 4, set(Out, Tape, case Arg1 =:= Arg2 of true -> 1; false -> 0 end), RelativeBase, Inputs, Outputs);
        9 ->
            {Arg} = GetParams({1, true}),
            run(I + 2, Tape, RelativeBase + Arg, Inputs, Outputs)
    end.

get(I, Tuple) -> 
    element(I + 1, Tuple).
set(I, Tuple, New) -> 
    setelement(I + 1, Tuple, New).

get_params(N, InitialI, Tape, ParameterModes, RelativeBase, IgnoreWriting) ->
    FullParameterModes = fill_string_right(ParameterModes, $0, N),
    io:format("~p~n", [RelativeBase]),
    Args = list_to_tuple([case Mode of $0 -> get(get(I, Tape), Tape); $1 -> get(I, Tape); $2 -> get(get(I + RelativeBase, Tape), Tape) end || {Mode, I} <- lists:zip(FullParameterModes, lists:seq(InitialI + 1, InitialI + N))]),
    case IgnoreWriting of true -> Args; false -> setelement(N, Args, get(InitialI + N, Tape)) end.

fill_string_right(Str, _, N) when length(Str) =:= N -> 
    Str;
fill_string_right(Str, Char, N) -> 
    fill_string_right(Str ++ [Char], Char, N).