package com.techempower;

import static com.techempower.Util.randomWorld;
import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import com.fizzed.rocker.RockerOutputFactory;
import io.jooby.Context;
import io.jooby.Jooby;
import io.jooby.MediaType;
import io.jooby.ServerOptions;
import io.jooby.rocker.ByteBufferOutput;
import io.jooby.rocker.RockerModule;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.SqlConnection;
import io.vertx.sqlclient.Tuple;

public class ReactivePg extends Jooby {

  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

  {
    /** Reduce the number of resources due we do reactive processing. */
    setServerOptions(
        new ServerOptions()
            .setIoThreads(Runtime.getRuntime().availableProcessors() + 1)
            .setWorkerThreads(Runtime.getRuntime().availableProcessors() + 1)
    );

    /** PG client: */
    PgClients clients = new PgClients(getConfig().getConfig("db"));

    /** Template engine: */
    install(new RockerModule().reuseBuffer(true));

    /** Single query: */
    get("/db", ctx -> {
      clients.next().preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld()), rsp -> {
        if (rsp.succeeded()) {
          RowIterator<Row> rs = rsp.result().iterator();
          Row row = rs.next();
          ctx.setResponseType(JSON)
              .send(Json.encode(new World(row.getInteger(0), row.getInteger(1))));
        } else {
          ctx.sendError(rsp.cause());
        }
      });
      return ctx;
    });

    /** Multiple queries: */
    get("/queries", ctx -> {
      int queries = Util.queries(ctx);
      AtomicInteger counter = new AtomicInteger();
      World[] result = new World[queries];
      PgPool client = clients.next();
      for (int i = 0; i < result.length; i++) {
        client.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld()), rsp -> {
          if (rsp.succeeded()) {
            RowIterator<Row> rs = rsp.result().iterator();
            Row row = rs.next();
            result[counter.get()] = new World(row.getInteger(0), row.getInteger(1));
          } else {
            sendError(ctx, rsp.cause());
          }
          // ready?
          if (counter.incrementAndGet() == queries) {
            ctx.setResponseType(JSON)
                .send(Json.encode(result));
          }
        });
      }
      return ctx;
    });

    /** Update queries: */
    get("/updates", ctx -> {
      int queries = Util.queries(ctx);
      World[] result = new World[queries];
      AtomicInteger counter = new AtomicInteger(0);
      PgPool pool = clients.next();
      pool.getConnection(connectCallback -> {
        if (connectCallback.failed()) {
          sendError(ctx, connectCallback.cause());
          return;
        }
        SqlConnection conn = connectCallback.result();
        for (int i = 0; i < queries; i++) {
          int id = randomWorld();
          conn.preparedQuery(SELECT_WORLD).execute(Tuple.of(id), selectCallback -> {
            if (selectCallback.failed()) {
              conn.close();
              sendError(ctx, selectCallback.cause());
              return;
            }
            result[counter.get()] = new World(
                selectCallback.result().iterator().next().getInteger(0),
                randomWorld());
            if (counter.incrementAndGet() == queries) {
              // Sort results... avoid dead locks
              Arrays.sort(result);
              List<Tuple> batch = new ArrayList<>(queries);
              for (World world : result) {
                batch.add(Tuple.of(world.getRandomNumber(), world.getId()));
              }

              conn.preparedQuery(UPDATE_WORLD).executeBatch(batch, updateCallback -> {
                if (updateCallback.failed()) {
                  sendError(ctx, updateCallback.cause());
                } else {
                  ctx.setResponseType(JSON)
                      .send(Json.encode(result));
                }
                conn.close();
              });
            }
          });
        }
      });
      return ctx;
    });

    /** Fortunes: */
    RockerOutputFactory<ByteBufferOutput> factory = require(RockerOutputFactory.class);
    get("/fortunes", ctx -> {
      clients.next().preparedQuery(SELECT_FORTUNE).execute(rsp -> {
        if (rsp.succeeded()) {
          RowIterator<Row> rs = rsp.result().iterator();
          List<Fortune> fortunes = new ArrayList<>();

          while (rs.hasNext()) {
            Row row = rs.next();
            fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
          }

          fortunes.add(new Fortune(0, "Additional fortune added at request time."));
          Collections.sort(fortunes);

          /** render view: */
          views.fortunes template = views.fortunes.template(fortunes);
          ctx.setResponseType(MediaType.html)
              .send(template.render(factory).toBuffer());
        } else {
          ctx.sendError(rsp.cause());
        }
      });
      return ctx;
    });
  }

  private void sendError(Context ctx, Throwable cause) {
    if (!ctx.isResponseStarted()) {
      ctx.sendError(cause);
    }
  }

  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, ReactivePg::new);
  }
}
