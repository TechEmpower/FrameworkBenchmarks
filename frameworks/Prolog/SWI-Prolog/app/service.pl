:- module(service, [random_number/1, 
                    random_numbers/2,
                    random_numbers_cached/2,
                    fortunes/1]).

:- use_module(database).

:- use_module(library(odbc)).
:- use_module(library(solution_sequences)).


random_number(Row) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        database:find_random_numbers(Connection, 1, [Row], false),
        odbc_disconnect(Connection)
    ).

random_numbers(0, []).
random_numbers(N, Rows) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        database:find_random_numbers(Connection, N, Rows, false),
        odbc_disconnect(Connection)
    ).

random_numbers_cached(0, []).
random_numbers_cached(N, Rows) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        database:find_random_numbers(Connection, N, Rows, true),
        odbc_disconnect(Connection)
    ).

fortunes(Rows) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        database:find_fortunes(Connection, Rows0),
        odbc_disconnect(Connection)
    ),
    Rows1 = [row(0, 'Additional fortune added at request time.')|Rows0],
    maplist(fortune_to_pair, Rows1, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Rows).

update(0, []).
update(N, Rows) :-
    setup_call_cleanup(
        odbc_connect('benchmark', Connection, []),
        ( database:find_random_numbers(Connection, N, Rows0, false)
        , database:update_random_numbers(Connection, Rows0, Rows)
        ),
        odbc_disconnect(Connection)
    ).

% -----------------------------------------------------------

fortune_to_pair(row(Id, Message), Message-row(Id, Message)).
