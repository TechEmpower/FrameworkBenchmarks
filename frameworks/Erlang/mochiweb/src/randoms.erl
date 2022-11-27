-module(randoms).
-export([init/0, random_id/0, random_ids/1, find/1, update/1]).

-define(world_max_records, 10000).

init() ->
    emysql:prepare(find_random, <<"SELECT id, randomNumber FROM World WHERE id = ? LIMIT 1;">>),
    emysql:prepare(update_random, <<"UPDATE World SET randomNumber = ? WHERE id = ? LIMIT 1;">>).

random_id() -> rand:uniform(?world_max_records).

random_ids(Times) -> random_ids(Times, []).

random_ids(0,     Acc) -> Acc;
random_ids(Times, Acc) -> random_ids(Times - 1, [random(Acc) | Acc]).

find(Id) ->
    [[Id, RandomNumber]] = store:rows(find_random, [Id]),
    {Id, RandomNumber}.

update(Id) ->
    NewRandomNumber = rand:uniform(10000),
    emysql:execute(db_pool, update_random, [NewRandomNumber, Id]),
    {Id, NewRandomNumber}.

random(List) ->
    Rand = random_id(),
    case lists:member(Rand, List) of
        true -> random(List);
        false -> Rand
    end.
