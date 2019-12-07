-module(concurrent).
-export([map/2, filter/2, filtermap/2]).

map(Fun, List) ->
    Listen = fun 
        Listen([H | T]) ->
            receive
                {H, Val} -> [Val | Listen(T)]
            end;
        Listen([]) ->
            []
    end,
    Pid = self(),
    Listen(lists:map(fun(Ele) ->
        spawn(fun() ->
            Pid ! {self(), Fun(Ele)}
        end)
    end, List)).

filter(Pred, List) ->
    Listen = fun 
        Listen([H | T], Result) ->
            receive
                H -> Listen(T, Result);
                {H, Val} -> Listen(T, [Val | Result])
            end;
        Listen([], Result) ->
            lists:reverse(Result)
    end,
    Pid = self(),
    Listen(lists:map(fun(Ele) ->
        spawn(fun() ->
            Pid ! case Pred(Ele) of
                true -> {self(), Ele};
                false -> self()
            end
        end)
    end, List), []).

filtermap(Fun, List) ->
    Listen = fun 
        Listen([H | T], Result) ->
            receive
                H -> Listen(T, Result);
                {H, Val} -> Listen(T, [Val | Result])
            end;
        Listen([], Result) ->
            lists:reverse(Result)
    end,
    Pid = self(),
    Listen(lists:map(fun(Ele) ->
        spawn(fun() ->
            Pid ! case Fun(Ele) of
                true -> {self(), Ele};
                {true, NewVal} -> {self(), NewVal};
                false -> self()
            end
        end)
    end, List), []).