-module(erl_bench).
-export([hello_json/0, hello_plain/0, random_json/0, randoms_json/1, update_randoms_json/1, fortunes_html/0]).

hello_json() ->
    {[{<<"message">>, <<"Hello, World!">>}]}.

hello_plain() ->
    <<"Hello, world!">>.

random_json() ->
    RandomId = randoms:random_id(),
    find(RandomId).

randoms_json(Count) ->
    [find(Id) || Id <- randoms:random_ids(Count)].

update_randoms_json(Count) ->
    [update(Id) || Id <- randoms:random_ids(Count)].

fortunes_html() ->
    Props = [[{message, Message}, {id, Id}] || {Message, Id} <- fortunes:all()],
    {ok, Html} = fortunes_view:render([{fortunes, Props}]),
    Html.

%% private

find(Id) ->
    {Id, RandomNumber} = randoms:find(Id),
    {[{<<"id">>, Id}, {<<"randomNumber">>, RandomNumber}]}.

update(Id) ->
    {Id, RandomNumber} = randoms:update(Id),
    {[{<<"id">>, Id}, {<<"randomNumber">>, RandomNumber}]}.
