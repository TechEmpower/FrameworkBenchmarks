-module(store).
-include_lib("emysql/include/emysql.hrl").
-export([init/0, rows/2]).

init() ->
    PoolSize = 256,
    User = "benchmarkdbuser",
    Password = "benchmarkdbpass",
    Host = "tfb-database",
    Port = 3306,
    Database = "hello_world",

    emysql:add_pool(db_pool, PoolSize, User, Password, Host, Port, Database, utf8),

    randoms:init(),
    fortunes:init().

rows(Statement, Query) ->
    Result = emysql:execute(db_pool, Statement, Query),
    Result#result_packet.rows.
