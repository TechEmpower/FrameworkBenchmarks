-module(mochiweb_bench_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

init([Options]) ->
    Web = worker(web_handler, Options),
    Processes = [Web],
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, Processes}}.

worker(Mod, Options) ->
    {Mod,
     {Mod, start, [Options]},
     permanent, 5000, worker, dynamic}.
