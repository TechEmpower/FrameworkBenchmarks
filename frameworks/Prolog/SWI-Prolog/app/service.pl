:- module(service, [random_number/1, 
                    random_numbers/2,
                    random_numbers_cached/2,
                    fortunes/1]).

:- use_module(database).
:- use_module(library(odbc)).

:- table random_numbers_cached/2.

random_number(Row) :-
    with_connection(Connection, database:find_random_numbers(Connection, 1, [Row])).

random_number(Row) :-
    with_connection(Connection, database:find_random_numbers(Connection, 1, [Row])).

random_numbers(0, []).
random_numbers(N, Rows) :-
    with_connection(Connection, database:find_random_numbers(Connection, N, Rows)).

random_numbers_cached(N, Rows) :-
    random_numbers(N, Rows).

fortunes(Rows) :-
    with_connection(Connection, database:find_fortunes(Connection, Rows0)),
    Rows1 = [row(0, 'Additional fortune added at request time.')|Rows0],
    maplist(fortune_to_pair, Rows1, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Rows).

update(0, []).
update(N, Rows) :-
    with_connection(Connection, (
        database:find_random_numbers(Connection, N, Rows0),
        database:update_random_numbers(Connection, Rows0, Rows)
    )).

fortune_to_pair(row(Id, Message), Message-row(Id, Message)).

with_connection(Connection, Goal) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        Goal,
        odbc_disconnect(Connection)
    ).