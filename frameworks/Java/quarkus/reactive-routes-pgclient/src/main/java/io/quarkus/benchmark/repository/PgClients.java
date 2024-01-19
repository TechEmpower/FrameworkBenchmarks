package io.quarkus.benchmark.repository;

import io.netty.util.concurrent.FastThreadLocal;
import io.vertx.mutiny.sqlclient.SqlClient;

class PgClients {
    private final FastThreadLocal<SqlClient> sqlClient = new FastThreadLocal<>() {
        @Override
        protected void onRemoval(final SqlClient value) {
            if (value != null) {
                value.close();
            }
        }
    };
    private PgClientFactory pgClientFactory;

    // for ArC
    public PgClients() {
    }

    public PgClients(final PgClientFactory pgClientFactory) {
        this.pgClientFactory = pgClientFactory;
    }

    SqlClient getClient() {
        SqlClient ret = sqlClient.get();
        if (ret == null) {
            ret = pgClientFactory.sqlClient(1);
            sqlClient.set(ret);
        }
        return ret;
    }
}