:- module(server, [server/1]).

:- use_module(service).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dyn_workers)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).

server(Port) :-
    odbc_set_option(connection_pooling(true)),
    current_prolog_flag(cpu_count, Cores),
    Workers is 256 * Cores,
    http_server(http_dispatch, [workers(Workers), port(Port), timeout(30)]).


:- http_handler('/plaintext',     plaintext_handler,     [chunked]).
:- http_handler('/json',          json_handler,          [chunked]).
:- http_handler('/db',            db_handler,            [chunked]).
:- http_handler('/queries',       queries_handler,       [chunked]).
:- http_handler('/fortunes',      fortunes_handler,      [chunked]).
:- http_handler('/updates',       updates_handler,       [chunked]).
:- http_handler('/cached-worlds', cached_worlds_handler, [chunked]).


plaintext_handler(_Request) :-
    format('Server: SWI-Prolog~n'),
    format('Content-Type: text/plain~n~n'),
    format('Hello, World!').

json_handler(_Request) :-
    format('Server: SWI-Prolog~n'),
    reply_json_dict(_{message: 'Hello, World!'}).

db_handler(_Request) :-
    service:random_number(Row),
    world_json(Row, Json),
    format('Server: SWI-Prolog~n'),
    reply_json_dict(Json).

queries_handler(Request) :-
    queries(Request, N),
    service:random_numbers(N, Rows),
    maplist(world_json, Rows, Json),
    format('Server: SWI-Prolog~n'),
    reply_json_dict(Json).

fortunes_handler(_Request) :-
    service:fortunes(Rows),
    format('Server: SWI-Prolog~n'),
    format('Content-Type: text/html; charset=utf-8~n~n'),
    phrase(page([ head(title('Fortunes')),
                  body(table(
                      [tr([th('id'), th('message')]),
                       \sequence(row, Rows)]))
                ]),
           Tokens),
    print_html(Tokens).

row(row(N, C)) -->
    html(tr([td(N), td(C)])).

updates_handler(Request) :-
    queries(Request, N),
    service:update(N, Rows),
    maplist(world_json, Rows, Json),
    format('Server: SWI-Prolog~n'),
    reply_json_dict(Json).

cached_worlds_handler(Request) :-
    queries(Request, N),
    service:random_numbers_cached(N, Rows),
    maplist(world_json, Rows, Json),
    format('Server: SWI-Prolog~n'),
    reply_json_dict(Json).

% -----------------------------------------------------------------------

queries(Request, Queries) :-
    catch(
        (   http_parameters(Request, [queries(Value, [integer, optional(true), default(1)])])
        ,   cut_off(Value, 1, 500, Queries)
        ),
        _Caught,
        Queries = 1
    ).

cut_off(V, L, _, L) :- V < L.
cut_off(V, _, U, U) :- V > U.
cut_off(V, _, _, V).

world_json(row(Id, RandomNumber), _{ id: Id, randomNumber: RandomNumber }).
