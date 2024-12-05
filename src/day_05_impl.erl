-module(day_05_impl).

-export([part_1/1, part_2/1]).

-spec part_1(string()) -> integer().
part_1(In) ->
    {Rules, Lists} = get_parts(In),
    Sort = create_sort_from_rules(Rules),
    Filtered = lists:filter(fun(X) -> X =:= lists:sort(Sort, X) end, Lists),
    sum_middles(Filtered).

-spec part_2(string()) -> integer().
part_2(In) ->
    {Rules, Lists} = get_parts(In),
    Sort = create_sort_from_rules(Rules),
    Filtered = lists:filter(fun(X) -> X =/= lists:sort(Sort, X) end, Lists),
    Sorted = lists:map(fun(X) -> lists:sort(Sort, X) end, Filtered),
    sum_middles(Sorted).

get_parts(In) ->
    [Rules, Lists] =
        lists:map(fun(X) -> string:split(X, "\n", all) end,
                  [X || X <- string:split(In, "\n\n", all), string:length(X) > 0]),
    {lists:map(fun(X) ->
                  [First, Last] = string:split(X, "|", all),
                  {First, Last}
               end,
               Rules),
     lists:map(fun(X) -> string:split(X, ",", all) end, Lists)}.

sum_middles(Lists) ->
    Middles = lists:map(fun(X) -> lists:nth((list_length(X) + 1) div 2, X) end, Lists),
    lists:sum(
        lists:map(fun(X) -> binary_to_integer(X) end, Middles)).

create_sort_from_rules(Rules) ->
    fun(X, Y) -> not lists:any(fun(Z) -> Z == {Y, X} end, Rules) end.

list_length(List) ->
    list_length(List, 0).

list_length([_ | T], Acc) ->
    list_length(T, Acc + 1);
list_length([], Acc) ->
    Acc.
