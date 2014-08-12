-module(yaws_bench_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

init([Options]) ->
    Web = worker(web_handler, Options),
    Processes = [Web],
    Strategy = {one_for_all, 0, 1},
    {ok, {Strategy, Processes}}.

worker(Mod, Options) ->
    {Mod,
     {Mod, start, [Options]},
     permanent, 2000, worker, [Mod]}.
