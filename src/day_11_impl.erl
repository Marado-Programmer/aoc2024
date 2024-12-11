-module(day_11_impl).

-export([part_1/1, part_2/1]).

-spec part_1(string()) -> integer().
part_1(In) ->
    Init =
        lists:map(fun binary_to_integer/1,
                  string:split(
                      string:trim(In), " ", all)),
    Final = blink_n(Init, 25),
    io:format("~p\n", [Final]),
    list_length(Final).

-spec part_2(string()) -> integer().
part_2(In) ->
    Init =
        lists:map(fun binary_to_integer/1,
                  string:split(
                      string:trim(In), " ", all)),
    Final = blink_n(Init, 75), % TOO MUCH
    io:format("~p\n", [Final]),
    list_length(Final).

blink_n(L, 0) ->
    L;
blink_n(L, N) ->
    io:format("~p\n", [N]),
    blink_n(blink(L), N - 1).

blink(L) ->
    blink(L, []).

blink([], Acc) ->
    lists:reverse(Acc);
blink([0 | T], Acc) ->
    blink(T, [1 | Acc]);
blink([H | T], Acc) ->
    Digits =
        trunc(math:ceil(
                  math:log10(H + 1))),
    if Digits rem 2 /= 0 ->
           blink(T, [H * 2024 | Acc]);
       true ->
           Power = trunc(math:pow(10, Digits div 2)),
           blink(T, [H rem Power, H div Power | Acc])
    end.

list_length(List) ->
    list_length(List, 0).

list_length([_ | T], Acc) ->
    list_length(T, Acc + 1);
list_length([], Acc) ->
    Acc.
