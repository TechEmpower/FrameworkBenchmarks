:- module(database, [find_random_numbers/4,
                     update_random_numbers/3,
                     find_fortunes/2]).

:- use_module(library(odbc)).
:- use_module(library(random)).

:- dynamic cache/2.

top_id(10001).

find_random_numbers(_Connection, 0, [], _Cached).
find_random_numbers(Connection, N, Rows, Cached) :-
    world_by_id_statement(Connection, Statement),
    find_random_numbers_(Statement, N, Rows, Cached).

find_random_numbers_(_Statement, 0, [], _Cached).
find_random_numbers_(Statement, N, [Row|Rows], Cached) :-
    N > 0,
    top_id(Top),
    random(1, Top, Id),
    odbc_execute_cached(Statement, [Id], Row, Cached),
    N1 is N - 1,
    find_random_numbers_(Statement, N1, Rows, Cached).

find_fortunes(Connection, Rows) :-
    fortune_statement(Connection, Statement),
    findall(Row, odbc_execute(Statement, [], Row), Rows).

update_random_numbers(_Connection, [], []).
update_random_numbers(Connection, Rows0, Rows) :-
    update_world_statement(Connection, Statement),
    update_random_numbers_(Statement, Rows0, Rows).

update_random_numbers_(_Statement, [], []).
update_random_numbers_(Statement, [row(Id0,_)|Rows0], [Row|Rows]) :-
    top_id(Top),
    random(1, Top, RandomNumber),
    Row = row(Id0, RandomNumber),
    odbc_execute(Statement, [RandomNumber, Id0]),
    update_random_numbers_(Statement, Rows0, Rows).

% ------------------------------------------------------------------------------------

world_by_id_statement(Connection, Statement) :-
    odbc_prepare(Connection, 'SELECT id, randomNumber FROM World WHERE id = ?', [integer], Statement).

fortune_statement(Connection, Statement) :-
    odbc_prepare(Connection, 'SELECT id, message FROM Fortune', [], Statement).

update_world_statement(Connection, Statement) :-
    odbc_prepare(Connection, 'UPDATE World SET randomNumber = ? WHERE id = ?', [integer, integer], Statement).

% ------------------------------------------------------------------------------------

odbc_execute_cached(Statement, Params, Row, true) :-
    ( cache(Params, Row)
    ; odbc_execute(Statement, Params, Row),
      assertz(cache(Params, Row))
    ).
odbc_execute_cached(Statement, Params, Row, false) :-
    odbc_execute(Statement, Params, Row).
