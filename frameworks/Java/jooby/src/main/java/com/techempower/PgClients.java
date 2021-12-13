package com.techempower;

import com.typesafe.config.Config;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;

public class PgClients {
  private static final PoolOptions SINGLE = new PoolOptions().setMaxSize(1);

  private final PgConnectOptions connectOptions;

  private final Vertx vertx;

  private ThreadLocal<PgPool> sqlClient = ThreadLocal.withInitial(this::sqlClientPool);

  public PgClients(Config config) {
    this.vertx = Vertx.vertx(new VertxOptions().setPreferNativeTransport(true).setWorkerPoolSize(4));
    this.connectOptions = pgPoolOptions(config);
  }

  public PgPool next() {
    return sqlClient.get();
  }

  private PgConnectOptions pgPoolOptions(Config config) {
    PgConnectOptions options = new PgConnectOptions();
    options.setDatabase(config.getString("databaseName"));
    options.setHost(config.getString("serverName"));
    options.setPort(config.getInt("portNumber"));
    options.setUser(config.getString("user"));
    options.setPassword(config.getString("password"));
    options.setCachePreparedStatements(true);
    return options;
  }

  private PgPool sqlClientPool() {
    return PgPool.pool(vertx, connectOptions, SINGLE);
  }
}
