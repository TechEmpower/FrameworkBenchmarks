:- module(database, [find_random_numbers/3,
                     update_random_numbers/3,
                     find_fortunes/2]).

:- use_module(library(odbc)).
:- use_module(library(random)).

top_id(10001).

find_random_numbers(_Connection, 0, []).
find_random_numbers(Connection, N, Rows) :-
    with_statement(Connection, world_by_id, Statement, find_random_numbers_(Statement, N, Rows)).

find_random_numbers_(_Statement, 0, []).
find_random_numbers_(Statement, N, [Row|Rows]) :-
    N > 0,
    top_id(Top),
    random(1, Top, Id),
    odbc_execute(Statement, [Id], Row),
    N1 is N - 1,
    find_random_numbers_(Statement, N1, Rows).

find_fortunes(Connection, Rows) :-
    with_statement(Connection, fortune, Statement, findall(Row, odbc_execute(Statement, [], Row), Rows)).

update_random_numbers(_Connection, [], []).
update_random_numbers(Connection, Rows0, Rows) :-
    with_statement(Connection, update_world, Statement, update_random_numbers_(Statement, Rows0, Rows)).

update_random_numbers_(_Statement, [], []).
update_random_numbers_(Statement, [row(Id0,_)|Rows0], [Row|Rows]) :-
    top_id(Top),
    random(1, Top, RandomNumber),
    Row = row(Id0, RandomNumber),
    odbc_execute(Statement, [RandomNumber, Id0]),
    update_random_numbers_(Statement, Rows0, Rows).

query(world_by_id,  'SELECT id, randomNumber FROM World WHERE id = ?', [integer]).
query(fortune,      'SELECT id, message FROM Fortune',                 []).
query(update_world, 'UPDATE World SET randomNumber = ? WHERE id = ?',  [integer, integer]).

with_statement(Connection, Name, Statement, Goal) :-
    setup_call_cleanup(
        (   query(Name, Query, Params)
        ,   odbc_prepare(Connection, Query, Params, Statement)
        ), 
        Goal,
        odbc_free_statement(Statement)
    ).
