-module(day6).
-export([fst/0, snd/0]).

fst() ->
    OrbitMap = get_orbit_map(),
    lists:foldl(fun(Object, Acc) ->
        Acc + length(get_orbits(Object, OrbitMap))
    end, 0, maps:keys(OrbitMap)).

snd() ->
    OrbitMap = get_orbit_map(),
    My_orbits = get_orbits("YOU", OrbitMap),
    Sn_orbits = get_orbits("SAN", OrbitMap),
    UpToCommonNode = fun(List) -> lists:takewhile(fun(E) -> E =/= first_common(My_orbits, Sn_orbits) end, List) end,
    length(UpToCommonNode(My_orbits)) + length(UpToCommonNode(Sn_orbits)).

get_orbit_map() ->
    {ok, Data} = file:read_file("day6.input"),
    maps:from_list([list_to_tuple(lists:reverse(string:split(Line, ")"))) || Line <- string:split(binary_to_list(Data), "\r\n", all)]).

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