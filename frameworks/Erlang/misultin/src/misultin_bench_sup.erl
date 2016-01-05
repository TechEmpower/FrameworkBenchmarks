-module(misultin_bench_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Options) ->
    supervisor:start_link(?MODULE, [Options]).

init([Options]) ->
    Misultin = supervisor(misultin, Options),
    Processes = [Misultin],
    Strategy = {one_for_one, 5, 30},
    {ok, {Strategy, Processes}}.

supervisor(Mod, Options) ->
    {Mod,
     {Mod, start_link, [Options]},
     permanent, infinity, supervisor, [Mod]}.
