package io.tadx.benchmark.db;

import io.tadx.core.TadxApplication;
import io.vertx.pgclient.PgBuilder;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.impl.SqlClientInternal;

public class PgConnPool {

    private static SqlClientInternal client;

    public static SqlClientInternal client() {
        if (client == null) {
            createClient();
        }
        return client;
    }

    private static synchronized void createClient() {
        if (client != null) {
            return;
        }
        PgConnectOptions connectOptions = new PgConnectOptions().
                setPort(5432).setHost("tfb-database").
                setDatabase("hello_world").
                setUser("benchmarkdbuser").
                setPassword("benchmarkdbpass").
                setCachePreparedStatements(true).
                setPreparedStatementCacheMaxSize(1024).
                setPipeliningLimit(100000);
        PoolOptions poolOptions = new PoolOptions().setMaxSize(1900);
        // Create the client pool
        client = (SqlClientInternal) PgBuilder.client().with(poolOptions).connectingTo(connectOptions).using(TadxApplication.vertx()).build();
    }
}
