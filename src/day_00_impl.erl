-module(day_00_impl).

-export([part_1/1]).

-spec part_1(string()) -> integer().
part_1(In) ->
    Lines = [X || X <- string:split(In, "\n", all), string:length(X) > 0],
    Values = find_calibration_values(Lines),
    %io:format("~p\n", [Values]),
    sum(Values).

find_calibration_values(List) ->
    find_calibration_values(List, []).

find_calibration_values([H | T], Acc) ->
    find_calibration_values(T, [calibration_value(H) | Acc]);
find_calibration_values([], Acc) ->
    Acc.

calibration_value(Line) ->
    first_digit_from_left(Line) * 10 + first_digit_from_left(reverse_binary(Line)).

first_digit_from_left(<<Char, Rest/binary>>) ->
    if (Char >= $0) and (Char =< $9) ->
           Char - $0;
       true ->
           first_digit_from_left(Rest)
    end;
first_digit_from_left(<<>>) ->
    -1.

reverse_binary(Binary) ->
    BinaryList = binary:bin_to_list(Binary),
    ReversedList = lists:reverse(BinaryList),
    list_to_binary(ReversedList).

sum(List) ->
    sum(List, 0).

sum([H | T], Acc) ->
    sum(T, Acc + H);
sum([], Acc) ->
    Acc.
