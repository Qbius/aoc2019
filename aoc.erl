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
    DayName = "days/day" ++ integer_to_list(Number) ++ ".erl",
    InputName = "inputs/day" ++ integer_to_list(Number) ++ ".input",
    case filelib:is_file(DayName) orelse filelib:is_file(InputName) of
        true ->
            "At least one of the files already exists!";
        false ->
            file:write_file(DayName, "-module(day" ++ integer_to_list(Number) ++ ").\n-export([fst/1, snd/1]).\n\nfst(Input) -> \n    unknown.\n\nsnd(Input) ->\n    unknown.\n\n-ifdef(puzzle_description)." ++ get_day_content(Number) ++ "\n-endif.", [append]),
            file:write_file(InputName, get_day_input(Number)),
            ready
    end.

aoc_get(Query) ->
    ssl:start(),
    inets:start(),
    {ok, AOCToken} = file:read_file("aoc_token"),
    Headers = [{"Cookie", "session=" ++ binary_to_list(AOCToken)}],
    HTTPOptions = [],
    Options = [],
    {ok, {_, _, Content}} = httpc:request(get, {"https://adventofcode.com/2019/" ++ Query, Headers}, HTTPOptions, Options),
    string:trim(Content).

get_day_content(Number) ->
    sanitize_source(aoc_get("day/" ++ integer_to_list(Number))).

get_day_input(Number) ->
    aoc_get("day/" ++ integer_to_list(Number) ++ "/input").

sanitize_source(Source) -> % this function is dirty! It's made specifically to handle AoC puzzle pages source code, alright?
    [_, AfterMain] = string:split(Source, "<main>"),
    [BeforeEndMain, _] = string:split(AfterMain, "</main>"),
    remove_html(lists:foldl(fun({From, To}, MidString) ->
        string:join(string:replace(MidString, From, To, all), "")
    end, BeforeEndMain, [{"</h2>", "\n"}, {"&amp;", "&"}, {"&lt;", "<"}, {"&gt;", ">"}, {"&quot;", "\""}])).

remove_html(Str) -> remove_html(Str, false, []).
remove_html([], _, Result) -> lists:reverse(Result);
remove_html([$<, $s, $c, $r, $i, $p, $t, $> | T], false, Result) ->
    remove_html(T, script_true, Result);
remove_html([$<, $/, $s, $c, $r, $i, $p, $t, $> | T], script_true, Result) ->
    remove_html(T, false, Result);
remove_html([$< | T], false, Result) ->
    case remove_prefixes(create_html_tag_list(["code", "div", "li", "span", "p", "h2", "pre", "ul", "article", "em", "a"]), T) of
        nomatch ->
            remove_html(T, false, [$<|Result]);
        TrimmedStr ->
            remove_html(TrimmedStr, true, Result)
    end;
remove_html([$> | T], true, Result) ->
    remove_html(T, false, Result);
remove_html([H | T], false, Result) ->
    remove_html(T, false, [H | Result]);
remove_html([_ | T], Other, Result) ->
    remove_html(T, Other, Result).

create_html_tag_list(BaseList) ->
    BaseList ++ [[$/ | Tag] || Tag <- BaseList].

remove_prefixes([Tag|T], Str) ->
    case string:prefix(Str, Tag) of
        nomatch -> remove_prefixes(T, Str);
        TrimmedStr -> TrimmedStr
    end;
remove_prefixes([], _) ->
    nomatch.