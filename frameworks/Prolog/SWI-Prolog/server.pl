:- module(server,
      [ server/1            % ?Port
      ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_dyn_workers)).

%!  server(+Port) is det.
%
%   Start the server at Port.

server(Port) :-
    server(Port,
           [ workers(10)
           ]).

server(Port, Options) :-
    http_server(http_dispatch,
                [ port(Port),
                  timeout(20)
                | Options
                ]).

:- http_handler('/plaintext', plaintext, [chunked]).
:- http_handler('/json', json, [chunked]).

plaintext(_Request) :-
    format('Server: SWI-Prolog~n'),
    format('Content-type: text/plain~n~n'),
    format('Hello, World!').

json(_Request) :-
    format('Server: SWI-Prolog~n'),
    format('Content-type: application/json~n~n'),
    format('{"message":"Hello, World!"}').

