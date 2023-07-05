package com.techempower;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;

import com.typesafe.config.Config;
import io.jooby.SneakyThrows;
import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.PreparedStatement;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.SqlClientInternal;

public class PgClient {
  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";
  private SqlClientInternal client;
  private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
  private PreparedQuery<RowSet<Row>> SELECT_FORTUNE_QUERY;
  private PreparedQuery<RowSet<Row>> UPDATE_WORLD_QUERY;

  public PgClient(Config config) {
    try {
      Vertx vertx = Vertx.vertx(
          new VertxOptions().setPreferNativeTransport(true).setWorkerPoolSize(4));
      PgConnectOptions connectOptions = pgPoolOptions(config);

      Future future = PgConnection.connect(vertx, connectOptions)
          .flatMap(conn -> {
            client = (SqlClientInternal) conn;
            Future<PreparedStatement> f1 = conn.prepare(SELECT_WORLD);
            Future<PreparedStatement> f2 = conn.prepare(SELECT_FORTUNE);
            Future<PreparedStatement> f3 = conn.prepare(UPDATE_WORLD);
            f1.onSuccess(ps -> SELECT_WORLD_QUERY = ps.query());
            f2.onSuccess(ps -> SELECT_FORTUNE_QUERY = ps.query());
            f3.onSuccess(ps -> UPDATE_WORLD_QUERY = ps.query());
            return Future.all(f1, f2, f3);
          })
          .toCompletionStage()
          .toCompletableFuture()
          .get();
      Throwable cause = future.cause();
      if (cause != null) {
        throw SneakyThrows.propagate(cause);
      }
    } catch (InterruptedException | ExecutionException cause) {
      throw SneakyThrows.propagate(cause);
    }
  }

  public void selectWorld(Tuple row, Handler<AsyncResult<RowSet<Row>>> handler) {
    SELECT_WORLD_QUERY.execute(row, handler);
  }

  public void selectWorldQuery(int queries,
      BiConsumer<Integer, PreparedQuery<RowSet<Row>>> consumer) {
    client.group(c -> {
      for (int i = 0; i < queries; i++) {
        consumer.accept(i, c.preparedQuery(SELECT_WORLD));
      }
    });
  }

  public PreparedQuery<RowSet<Row>> fortuneQuery() {
    return SELECT_FORTUNE_QUERY;
  }

  public void selectWorldForUpdate(int queries,
      BiConsumer<Integer, PreparedQuery<RowSet<Row>>> consumer) {
    client.group(c -> {
      PreparedQuery<RowSet<Row>> statement = c.preparedQuery(SELECT_WORLD);
      for (int i = 0; i < queries; i++) {
        consumer.accept(i, statement);
      }
    });
  }

  public void updateWorld(List<Tuple> batch, Handler<AsyncResult<RowSet<Row>>> handler) {
    UPDATE_WORLD_QUERY.executeBatch(batch, handler);
  }

  private PgConnectOptions pgPoolOptions(Config config) {
    PgConnectOptions options = new PgConnectOptions();
    options.setDatabase(config.getString("databaseName"));
    options.setHost(config.getString("serverName"));
    options.setPort(config.getInt("portNumber"));
    options.setUser(config.getString("user"));
    options.setPassword(config.getString("password"));
    options.setCachePreparedStatements(true);
    // Large pipelining means less flushing and we use a single connection anyway
    options.setPipeliningLimit(100_000);
    return options;
  }
}
