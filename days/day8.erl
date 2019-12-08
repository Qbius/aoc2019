-module(day8).
-export([fst/1, snd/1]).

fst(Input) ->
    ImageWidth = 25,
    ImageHeight = 6,
    [{_ZeroCount, OnesTwosCount} | _] = lists:sort(concurrent:map(fun(LayerPixels) ->
        {count($0, LayerPixels), count($1, LayerPixels) * count($2, LayerPixels)}
    end, list_to_chunks(binary_to_list(Input), ImageWidth * ImageHeight))),
    OnesTwosCount.

snd(Input) ->
    ImageWidth = 25,
    ImageHeight = 6,
    LayeredPixels = rotate_lists(list_to_chunks(binary_to_list(Input), ImageWidth * ImageHeight)),
    Image = concurrent:map(fun
        FirstNotTwo([$2 | T]) -> FirstNotTwo(T);
        FirstNotTwo([NT | _]) -> NT
    end, LayeredPixels),
    ImageRows = list_to_chunks(Image, ImageWidth),
    lists:foreach(fun(Row) -> io:format("~p~n", [[case Pixel of $1 -> $X; $0 -> 32 end || Pixel <- Row]]) end, ImageRows),
    "displayed above".

list_to_chunks(BaseList, ChunkSize) ->
  list_to_chunks(BaseList, ChunkSize, []).
list_to_chunks([], _, Result) ->
    lists:reverse(Result);
list_to_chunks(BaseList, ChunkSize, Result) ->
    {Layer, Rest} = lists:split(ChunkSize, BaseList),
    list_to_chunks(Rest, ChunkSize, [Layer | Result]).

count(Ele, List) ->
    count(Ele, List, 0).
count(Ele, [Ele | T], Count) ->
    count(Ele, T, Count + 1);
count(Ele, [_ | T], Count) ->
    count(Ele, T, Count);
count(_, [], Count) ->
    Count.

rotate_lists(Lists) ->
    rotate_lists(Lists, []).
rotate_lists(Lists = [Head | _], Result) when length(Head) > 0 ->
    {Hs, Ts} = lists:foldl(fun([H|T], {Hs, Ts}) ->
        {[H | Hs], [T | Ts]}
    end, {[], []}, Lists),
    rotate_lists(Ts, [Hs | Result]);
rotate_lists(_, Result) ->
    lists:reverse(Result).

-ifdef(puzzle_description).

--- Day 8: Space Image Format ---
The Elves' spirits are lifted when they realize you have an opportunity to reboot one of their Mars rovers, and so they are curious if you would spend a brief sojourn on Mars. You land your ship near the rover.
When you reach the rover, you discover that it's already in the process of rebooting! It's just waiting for someone to enter a BIOS password. The Elf responsible for the rover takes a picture of the password (your puzzle input) and sends it to you via the Digital Sending Network.
Unfortunately, images sent via the Digital Sending Network aren't encoded with any normal encoding; instead, they're encoded in a special Space Image Format.  None of the Elves seem to remember why this is the case. They send you the instructions to decode it.
Images are sent as a series of digits that each represent the color of a single pixel.  The digits fill each row of the image left-to-right, then move downward to the next row, filling rows top-to-bottom until every pixel of the image is filled.
Each image actually consists of a series of identically-sized layers that are filled in this way. So, the first digit corresponds to the top-left pixel of the first layer, the second digit corresponds to the pixel to the right of that on the same layer, and so on until the last digit, which corresponds to the bottom-right pixel of the last layer.
For example, given an image 3 pixels wide and 2 pixels tall, the image data 123456789012 corresponds to the following image layers:
Layer 1: 123
         456

Layer 2: 789
       012

The image you received is 25 pixels wide and 6 pixels tall.
To make sure the image wasn't corrupted during transmission, the Elves would like you to find the layer that contains the fewest 0 digits.  On that layer, what is the number of 1 digits multiplied by the number of 2 digits.

-endif.