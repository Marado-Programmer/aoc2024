-module(day_09_impl).

-export([part_1/1, part_2/1]).

-spec part_1(string()) -> integer().
part_1(In) ->
    Digits =
        lists:map(fun(X) -> X - $0 end,
                  string:to_graphemes(
                      string:trim(In))),
    {Disk, _} = create_disk(Digits),
    {_, Checksum} =
        lists:foldl(fun(X, {ID, Acc}) -> {ID + 1, X * ID + Acc} end,
                    {0, 0},
                    update_filesystem_checksum(Disk)),
    Checksum.

-spec part_2(string()) -> integer().
part_2(In) ->
    Digits =
        lists:map(fun(X) -> X - $0 end,
                  string:to_graphemes(
                      string:trim(In))),
    {Disk, LastID} = create_disk(Digits),
    {_, Checksum} =
        lists:foldl(fun(X, {ID, Acc}) ->
                       {ID + 1,
                        case X of
                            nil -> 0;
                            _ -> X
                        end
                        * ID
                        + Acc}
                    end,
                    {0, 0},
                    update_filesystem_checksum_whole_file(Disk, LastID)),
    Checksum.

create_disk(DiskMap) ->
    create_disk(DiskMap, 0, file, []).

create_disk([], ID, _, Disk) ->
    {lists:reverse(Disk), ID - 1};
create_disk([Head | Rest], ID, file, Disk) ->
    create_disk(Rest, ID + 1, free, add_n_times(ID, Head, Disk));
create_disk([Head | Rest], ID, free, Disk) ->
    create_disk(Rest, ID, file, add_n_times(nil, Head, Disk)).

add_n_times(_, 0, Acc) ->
    Acc;
add_n_times(X, N, Acc) ->
    add_n_times(X, N - 1, [X | Acc]).

update_filesystem_checksum(Disk) ->
    update_filesystem_checksum(Disk, []).

update_filesystem_checksum([], Acc) ->
    lists:reverse(Acc);
update_filesystem_checksum([nil | Rest], Acc) ->
    case remove_last_not_nil(Rest) of
        {[], nil} ->
            update_filesystem_checksum([], Acc);
        {L, Last} ->
            update_filesystem_checksum(L, [Last | Acc])
    end;
update_filesystem_checksum([Head | Rest], Acc) ->
    update_filesystem_checksum(Rest, [Head | Acc]).

remove_last_not_nil([]) ->
    {[], nil};
remove_last_not_nil(L) ->
    case lists:last(L) of
        nil ->
            remove_last_not_nil(lists:droplast(L));
        X ->
            {lists:droplast(L), X}
    end.

update_filesystem_checksum_whole_file(Disk, LastID) ->
    update_filesystem_checksum_whole_file(Disk, LastID, true).

update_filesystem_checksum_whole_file([], _, _) ->
    [];
update_filesystem_checksum_whole_file(Disk, 0, _) ->
    Disk;
update_filesystem_checksum_whole_file([X | _] = Disk, LastID, _) when X >= LastID ->
    Disk;
update_filesystem_checksum_whole_file(Disk, LastID, true) ->
    {Firsts, Seconds} = lists:splitwith(fun(X) -> X =/= nil end, Disk),
    {Nils, Lasts} = lists:splitwith(fun(X) -> X =:= nil end, Seconds),
    NilsL = list_length(Nils),
    IDs = lists:filter(fun(X) -> X =:= LastID end, Lasts),
    IDsL = list_length(IDs),
    Parts =
        if NilsL >= IDsL ->
               [Firsts,
                fill(IDs, NilsL),
                lists:map(fun(X) ->
                             if X =:= LastID -> nil;
                                true -> X
                             end
                          end,
                          Lasts)];
           true ->
               [Firsts, Nils, update_filesystem_checksum_whole_file(Lasts, LastID, false)]
        end,
    io:format("~p\n", [LastID]),
    update_filesystem_checksum_whole_file(lists:append(Parts), LastID - 1, true);
update_filesystem_checksum_whole_file(Disk, LastID, false) ->
    {Firsts, Seconds} = lists:splitwith(fun(X) -> X =/= nil end, Disk),
    {Nils, Lasts} = lists:splitwith(fun(X) -> X =:= nil end, Seconds),
    NilsL = list_length(Nils),
    IDs = lists:filter(fun(X) -> X =:= LastID end, Lasts),
    IDsL = list_length(IDs),
    lists:append(if NilsL >= IDsL ->
                        [Firsts,
                         fill(IDs, NilsL),
                         lists:map(fun(X) ->
                                      if X =:= LastID -> nil;
                                         true -> X
                                      end
                                   end,
                                   Lasts)];
                    true ->
                        [Firsts, Nils, update_filesystem_checksum_whole_file(Lasts, LastID, false)]
                 end).

fill(L, N) ->
    case list_length(L) of
        X when X < N ->
            fill([nil | L], N);
        _ ->
            lists:reverse(L)
    end.

list_length(List) ->
    list_length(List, 0).

list_length([_ | T], Acc) ->
    list_length(T, Acc + 1);
list_length([], Acc) ->
    Acc.
