-module(day6).
-export([fst/1, snd/1]).

fst(Input) ->
    OrbitMap = maps:from_list([list_to_tuple(lists:reverse(string:split(Line, ")"))) || Line <- string:split(binary_to_list(Input), "\r\n", all)]),
    lists:foldl(fun(Object, Acc) ->
        Acc + length(get_orbits(Object, OrbitMap))
    end, 0, maps:keys(OrbitMap)).

snd(Input) ->
    OrbitMap = maps:from_list([list_to_tuple(lists:reverse(string:split(Line, ")"))) || Line <- string:split(binary_to_list(Input), "\r\n", all)]),
    My_orbits = get_orbits("YOU", OrbitMap),
    Sn_orbits = get_orbits("SAN", OrbitMap),
    UpToCommonNode = fun(List) -> lists:takewhile(fun(E) -> E =/= first_common(My_orbits, Sn_orbits) end, List) end,
    length(UpToCommonNode(My_orbits)) + length(UpToCommonNode(Sn_orbits)).

get_orbits(Object, OrbitMap) ->
    case OrbitMap of
        #{Object := OrbitedObject} -> [OrbitedObject | get_orbits(OrbitedObject, OrbitMap)];
        _ -> []
    end.

first_common([H|T], OtherList) ->
    case lists:member(H, OtherList) of
        true -> H;
        false -> first_common(T, OtherList)
    end.