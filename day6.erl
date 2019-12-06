-module(day6).
-export([fst/0, snd/0]).

get_orbits(Object, OrbitMap) ->
    case OrbitMap of
        #{Object := OrbitedObject} -> 1 + get_orbits(OrbitedObject, OrbitMap);
        _ -> 0
    end.

fst() ->
    {ok, Data} = file:read_file("day6.input"),
    OrbitMap = maps:from_list([list_to_tuple(lists:reverse(string:split(Line, ")"))) || Line <- string:split(binary_to_list(Data), "\r\n", all)]),
    AllObjects = sets:to_list(sets:union(sets:from_list(maps:keys(OrbitMap)), sets:from_list(maps:values(OrbitMap)))),
    lists:foldl(fun(Object, Acc) ->
        Acc + get_orbits(Object, OrbitMap)
    end, 0, AllObjects).

snd() ->
    ok.