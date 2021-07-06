:- module(server, [server/1]).

:- use_module(service).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dyn_workers)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(st/st_render)).
:- use_module(library(http/thread_httpd)).


server(Port) :-
    odbc_set_option(connection_pooling(true)),
    current_prolog_flag(cpu_count, Cores),
    Workers is Cores * 2,
    server(Port, [workers(Workers)]).

server(Port, Options) :-
    http_server(http_dispatch, [port(Port),timeout(120)|Options]).


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
    maplist(fortune_json, Rows, Items),
    render_template(fortunes, _{ items: Items }, Payload, Len),
    format('Server: SWI-Prolog~n'),
    format('Content-Type: text/html; charset=utf-8~n'),
    format('Content-Length: ~d~n~n', [Len]),
    format(Payload).

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
        ( http_parameters(Request, [queries(Value, [integer, optional(true), default(1)])])
        , cut_off(Value, 1, 500, Queries)
        ),
        Caught,
        Queries = 1
    ).

cut_off(V, L, _, L) :- V < L.
cut_off(V, _, U, U) :- V > U.
cut_off(V, _, _, V).

world_json(row(Id, RandomNumber), _{ id: Id, randomNumber: RandomNumber }).

fortune_json(row(Id, Message), _{ id: Id, message: Message }).

render_template(Template, Data, Result, Len) :-
    with_output_to(codes(Codes), (
        current_output(Out),
        st_render_file(Template, Data, Out, _{ cache: true })
    )),
    length(Codes, Len),
    string_codes(Result, Codes).
