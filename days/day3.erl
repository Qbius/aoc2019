-module(day3).
-export([fst/0, snd/0]).

sign(0) -> 0;
sign(N) when N < 0 -> -1;
sign(_) -> 1.

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
        NewPointsSet = sets:from_list([{X, Y} || X <- lists:seq(AccX + sign(UpdatedX - AccX), UpdatedX, sign(UpdatedX - AccX)), Y <- lists:seq(AccY + sign(UpdatedY - AccY), UpdatedY, sign(UpdatedY - AccY))]),
         {sets:union(PointsSet, NewPointsSet), {UpdatedX, UpdatedY}}
    end, {sets:new(), {0, 0}}, lists:map(HandlePart, string:split(Input, ",", all))),
    FinalSet.

snd() ->
    {ok, Data} = file:read_file("day3.input"),
    [FirstWire, SecondWire] = string:split(binary_to_list(Data), "\r\n", all),
    {FirstWirePoints, FirstWireSteps} = get_wire_points_with_steps(FirstWire),
    {SecondWirePoints, SecondWireSteps} = get_wire_points_with_steps(SecondWire),
    lists:min([maps:get(Point, FirstWireSteps) + maps:get(Point, SecondWireSteps) || Point <- sets:to_list(sets:intersection(FirstWirePoints, SecondWirePoints))]).

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
        end, {PointsSet, PreviousSteps, PointsStepMap}, [{X, Y} || X <- lists:seq(AccX + sign(UpdatedX - AccX), UpdatedX, sign(UpdatedX - AccX)), Y <- lists:seq(AccY + sign(UpdatedY - AccY), UpdatedY, sign(UpdatedY - AccY))]),
         {UpdatedPointsSet, UpdatedExtraSteps, UpdatedPointsStepMap, {UpdatedX, UpdatedY}}
    end, {sets:new(), 1, maps:new(), {0, 0}}, lists:map(HandlePart, string:split(Input, ",", all))),
   {FinalSet, FinalMap}.