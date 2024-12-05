-module(day_02_impl).

-export([part_1/1, part_2/1]).

-spec part_1(string()) -> integer().
part_1(In) ->
    Safeties = get_safeties(In, 0),
    %io:format("~lp\n", [Safeties]),
    list_length([X || X <- Safeties, X]).

-spec part_2(string()) -> integer().
part_2(In) ->
    Safeties = get_safeties(In, 1),
    %io:format("~lp\n", [Safeties]),
    list_length([X || X <- Safeties, X]).

get_safeties(In, Tolerance) ->
    Lines = [X || X <- string:split(In, "\n", all), string:length(X) > 0],
    Reports = lists:map(fun(X) -> string:split(X, " ", all) end, Lines),
    Levels =
        lists:map(fun(X) ->
                     lists:map(fun(Y) ->
                                  {Int, _} = string:to_integer(Y),
                                  Int
                               end,
                               X)
                  end,
                  Reports),
    lists:map(fun(X) -> calc_report_safety(X, Tolerance) end, Levels).

calc_report_safety(Report, Tolerance) ->
    %io:format("\n"),
    calc_report_safety_loop([], Report, 0, Tolerance).

calc_report_safety_loop(_, _, _, Tolerance) when Tolerance < 0 ->
    false;
calc_report_safety_loop(_, [_], _, _) ->
    true;
calc_report_safety_loop(Previous, [X, Y | Rest] = B, Last, Tolerance) ->
    %io:format("A=~lp\tLast=~lp\tT=~lp\n", [B, Last, Tolerance]),
    Diff = X - Y,
    if
        (Last * Diff < 0) or (abs(Diff) < 1) or (abs(Diff) > 3) ->
            case Previous of
                [{P, PLast}|PreviousRest] -> maybe
                    false ?= calc_report_safety_loop(PreviousRest, [P,Y|Rest], PLast, Tolerance - 1),
                    false ?= calc_report_safety_loop(PreviousRest, [P,X|Rest], PLast, Tolerance - 1),
                    calc_report_safety_loop(PreviousRest, [X,Y|Rest], PLast, Tolerance - 1)
                end;
                [] -> maybe
                    false ?= calc_report_safety_loop(Previous, [Y|Rest], Last, Tolerance - 1),
                    calc_report_safety_loop(Previous, [X|Rest], Last, Tolerance - 1)
                end
            end;
        true -> calc_report_safety_loop([{X, Last}|Previous], [Y|Rest], Diff, Tolerance)
    end.

list_length(List) ->
    list_length(List, 0).

list_length([_ | T], Acc) ->
    list_length(T, Acc + 1);
list_length([], Acc) ->
    Acc.
