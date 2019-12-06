-module(aoc).
-export([day/1, new/1]).

day(Number) ->
    Filename = "days/day" ++ integer_to_list(Number) ++ ".erl",
    case compile:file(Filename, []) of
        {ok, Module} ->
            code:purge(Module),
            code:load_file(Module),
            {ok, Input} = file:read_file("inputs/day" ++ integer_to_list(Number) ++ ".input"),
            Start = os:system_time(millisecond),
            GetAnswer = fun(Fun) ->
                case Module:Fun(Input) of
                    unknown ->
                        unknown;
                    Result when is_integer(Result) ->
                        Answer = integer_to_list(Result),
                        Stop = os:system_time(millisecond),
                        Minutes = integer_to_list((Stop - Start) div 60000),
                        Seconds = case io_lib:format("~.3f", [((Stop - Start) rem 60000) / 1000]) of [S] -> S; S -> S end,
                        "The answer is " ++ Answer ++ ", time elapsed: " ++ Minutes ++ " minutes and " ++ Seconds ++ " seconds"
                end
            end,
            io:format("First part:~n~s~n", [GetAnswer(fst)]),
            io:format("Second part:~n~s~n", [GetAnswer(snd)]),
            finished;
        _ ->
            "Compilation of file " ++ Filename ++ " unsuccessful"
    end.

new(Number) ->
    Name = "days/day" ++ integer_to_list(Number) ++ ".erl",
    case filelib:is_file(Name) of
        true ->
            "File already exists!";
        false ->
            file:write_file(Name, "-module(day" ++ integer_to_list(Number) ++ ").\n-export([fst/1, snd/1]).\n\nfst(Input) -> \n    unknown.\n\nsnd(Input) ->\n    unknown.\n", [append]),
            ready
    end.