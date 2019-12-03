-module(day3).
-export([fst/0, snd/0]).

fst() ->
    {ok, Data} = file:read_file("day3.input"),
    [FirstWire, SecondWire] = string:split(binary_to_list(Data), "\r\n", all),
    lists:min([abs(X) + abs(Y) || {X, Y} <- sets:to_list(sets:intersection(get_wire_points(FirstWire), get_wire_points(SecondWire)))]).

get_wire_points(Input) ->
    HandlePart = fun([Letter | Number]) ->
        Change = list_to_integer(Number),
        case Letter of 
            $R -> {Change, 0}; 
            $L -> {-Change, 0}; 
            $U -> {0, Change}; 
            $D -> {0, -Change} 
        end
    end,
    {FinalSet, _WireEnd} = lists:foldl(fun({CurrX, CurrY}, {PointsSet, {AccX, AccY}}) ->
        {UpdatedX, UpdatedY} = {CurrX + AccX, CurrY + AccY},
        NewPointsSet = sets:from_list([{X, Y} || X <- lists:seq(min(UpdatedX, AccX), max(UpdatedX, AccX)), Y <- lists:seq(min(UpdatedY, AccY), max(UpdatedY, AccY))]),
         {sets:union(PointsSet, NewPointsSet), {UpdatedX, UpdatedY}}
    end, {sets:new(), {0, 0}}, lists:map(HandlePart, string:split(Input, ",", all))),
    sets:del_element({0, 0}, FinalSet).

snd() ->
    %{ok, Data} = file:read_file("day3.input"),
    %[FirstWire, SecondWire] = string:split(binary_to_list(Data), "\r\n", all),
    
    FirstWire = "R75,D30,R83,U83,L12,D49,R71,U7,L72",
    SecondWire = "U62,R66,U55,R34,D71,R55,D58,R83",
    {FirstWirePoints, FirstWireSteps} = get_wire_points_with_steps(FirstWire),
    {SecondWirePoints, _SecondWireSteps} = get_wire_points_with_steps(SecondWire),
    lists:min([maps:get(Point, FirstWireSteps) || Point <- sets:to_list(sets:intersection(FirstWirePoints, SecondWirePoints))]).

sign(0) -> 0;
sign(N) when N < 0 -> -1;
sign(_) -> 1.

get_wire_points_with_steps(Input) ->
    HandlePart = fun([Letter | Number]) ->
        Change = list_to_integer(Number),
        case Letter of 
            $R -> {Change, 0}; 
            $L -> {-Change, 0}; 
            $U -> {0, Change}; 
            $D -> {0, -Change} 
        end
    end,
    {FinalSet, _TotalSteps, FinalMap, _WireEnd} = lists:foldl(fun({CurrX, CurrY}, {PointsSet, PreviousSteps, PointsStepMap, {AccX, AccY}}) ->
        {UpdatedX, UpdatedY} = {CurrX + AccX, CurrY + AccY},
        {UpdatedPointsSet, UpdatedExtraSteps, UpdatedPointsStepMap} = lists:foldl(fun(NewPoint, {MidSet, ExtraSteps, MidMap}) ->
            UpdatedMidMap = case MidMap of
                #{NewPoint := _} -> MidMap;
                _ -> MidMap#{NewPoint => ExtraSteps}
            end,
            {sets:add_element(NewPoint, MidSet), ExtraSteps + 1, UpdatedMidMap}
        end, {PointsSet, PreviousSteps, PointsStepMap}, [{X, Y} || X <- lists:seq(AccX, UpdatedX, sign(UpdatedX - AccX)), Y <- lists:seq(AccY, UpdatedY, sign(UpdatedY - AccY))]),
         {UpdatedPointsSet, UpdatedExtraSteps, UpdatedPointsStepMap, {UpdatedX, UpdatedY}}
    end, {sets:new(), 0, maps:new(), {0, 0}}, lists:map(HandlePart, string:split(Input, ",", all))),
    {sets:del_element({0, 0}, FinalSet), FinalMap}.