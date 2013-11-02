-module(elli_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, 5000,
       "benchmarkdbuser", "benchmarkdbpass", "localhost", 3306,
       "hello_world", utf8),
    emysql:prepare(db_stmt, <<"SELECT * FROM World where id = ?">>),
    ElliOpts = [{callback, elli_bench_cb}, {port, 8080}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.

