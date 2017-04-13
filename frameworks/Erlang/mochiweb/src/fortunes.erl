-module(fortunes).
-export([init/0, all/0]).

init() ->
    emysql:prepare(all_fortunes, <<"SELECT message, id FROM Fortune;">>).

format_list({Message, Id}) ->
    lists:flatten(io_lib:format("(Message: ~s, Id: ~b)", [Message, Id])).

format_lists(Fortunes) ->
    string:join(lists:map(fun format_list/1, Fortunes), ", ").

all() ->
    Fortunes = [{Message, Id} || [Message, Id] <- store:rows(all_fortunes, [])] ++ [{<<"Additional fortune added at request time.">>, 0}],
    lists:sort(Fortunes).
