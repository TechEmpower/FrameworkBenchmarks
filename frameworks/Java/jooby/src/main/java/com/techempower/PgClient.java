package com.techempower;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;

import com.typesafe.config.Config;
import io.jooby.SneakyThrows;
import io.vertx.core.AsyncResult;
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

  static {
    // Should be all I/O processing for SQL responses
    System.setProperty("vertx.nettyIORatio", "100");
  }

  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

  private static class DbConnection {
    private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
    private PreparedQuery<RowSet<Row>> SELECT_FORTUNE_QUERY;
    private PreparedQuery<RowSet<Row>> UPDATE_WORLD_QUERY;
    private SqlClientInternal connection;
  }

  private static class DbConnectionFactory extends ThreadLocal<DbConnection> {

    private final PgConnectOptions options;

    public DbConnectionFactory(PgConnectOptions options) {
      this.options = options;
    }

    private <T> Handler<AsyncResult<T>> onSuccess(Handler<T> handler) {
      return ar -> {
        if (ar.succeeded()) {
          handler.handle(ar.result());
        }
      };
    }

    @Override protected DbConnection initialValue() {
      try {
        DbConnection result = new DbConnection();
        Vertx vertx = Vertx.vertx(
            new VertxOptions()
                .setPreferNativeTransport(true)
                .setEventLoopPoolSize(1)
                .setWorkerPoolSize(1)
                .setInternalBlockingPoolSize(1)
        );
        var future = PgConnection.connect(vertx, options)
            .flatMap(conn -> {
              result.connection = (SqlClientInternal) conn;
              Future<PreparedStatement> f1 = conn.prepare(SELECT_WORLD)
                  .andThen(onSuccess(ps -> result.SELECT_WORLD_QUERY = ps.query()));
              Future<PreparedStatement> f2 = conn.prepare(SELECT_FORTUNE)
                  .andThen(onSuccess(ps -> result.SELECT_FORTUNE_QUERY = ps.query()));
              Future<PreparedStatement> f3 = conn.prepare(UPDATE_WORLD)
                  .andThen(onSuccess(ps -> result.UPDATE_WORLD_QUERY = ps.query()));
              return Future.join(f1, f2, f3);
            })
            .toCompletionStage()
            .toCompletableFuture()
            .get();

        Throwable cause = future.cause();
        if (cause != null) {
          throw SneakyThrows.propagate(cause);
        }
        return result;
      } catch (InterruptedException ex) {
        Thread.currentThread().interrupt();
        throw SneakyThrows.propagate(ex);
      } catch (ExecutionException ex) {
        throw SneakyThrows.propagate(ex.getCause());
      }
    }
  }

  private final ThreadLocal<DbConnection> sqlClient;

  public PgClient(Config config) {
    this.sqlClient = new DbConnectionFactory(pgPoolOptions(config));
  }

  public void selectWorld(Tuple row, Handler<AsyncResult<RowSet<Row>>> handler) {
    this.sqlClient.get().SELECT_WORLD_QUERY.execute(row, handler);
  }

  public void selectWorlds(int queries, Handler<AsyncResult<RowSet<Row>>> handler) {
    this.sqlClient.get().connection.group(c -> {
      for (int i = 0; i < queries; i++) {
        c.preparedQuery(SELECT_WORLD).execute(Tuple.of(Util.randomWorld()), handler);
      }
    });
  }

  public void fortunes(Handler<AsyncResult<RowSet<Row>>> handler) {
    this.sqlClient.get().SELECT_FORTUNE_QUERY.execute(handler);
  }

  public void selectWorldForUpdate(int queries,
      BiConsumer<Integer, PreparedQuery<RowSet<Row>>> consumer) {
    this.sqlClient.get().connection.group(c -> {
      PreparedQuery<RowSet<Row>> statement = c.preparedQuery(SELECT_WORLD);
      for (int i = 0; i < queries; i++) {
        consumer.accept(i, statement);
      }
    });
  }

  public void updateWorld(List<Tuple> batch, Handler<AsyncResult<RowSet<Row>>> handler) {
    this.sqlClient.get().UPDATE_WORLD_QUERY.executeBatch(batch, handler);
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
