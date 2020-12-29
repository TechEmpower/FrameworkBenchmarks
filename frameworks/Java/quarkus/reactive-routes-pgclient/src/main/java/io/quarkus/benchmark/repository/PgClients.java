package io.quarkus.benchmark.repository;

import io.vertx.mutiny.pgclient.PgPool;
import io.vertx.mutiny.sqlclient.SqlClient;

class PgClients {
    private static final int POOL_SIZE = 4;

    private ThreadLocal<SqlClient> sqlClient = new ThreadLocal<>();
    private ThreadLocal<PgPool> pool = new ThreadLocal<>();
    private PgClientFactory pgClientFactory;

	// for ArC
	public PgClients() {
	}

	public PgClients(PgClientFactory pgClientFactory) {
	    this.pgClientFactory = pgClientFactory;
    }

    SqlClient getClient() {
        SqlClient ret = sqlClient.get();
        if(ret == null) {
            ret = pgClientFactory.sqlClient(1);
            sqlClient.set(ret);
        }
		return ret;
	}

	synchronized PgPool getPool() {
        PgPool ret = pool.get();
        if(ret == null) {
            ret = pgClientFactory.sqlClient(POOL_SIZE);
            pool.set(ret);
        }
        return ret;
	}
}