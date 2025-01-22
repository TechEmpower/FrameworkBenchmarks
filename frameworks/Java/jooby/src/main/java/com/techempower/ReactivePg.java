package com.techempower;

import static com.techempower.Util.randomWorld;
import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;

import java.util.*;

import com.fizzed.rocker.RockerOutputFactory;
import com.techempower.rocker.BufferRockerOutput;
import io.jooby.*;
import io.jooby.rocker.DataBufferOutput;
import io.jooby.rocker.RockerModule;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.Tuple;

public class ReactivePg extends Jooby {
  {
    /** Reduce the number of resources due we do reactive processing. */
    setServerOptions(
        new ServerOptions()
            .setIoThreads(Runtime.getRuntime().availableProcessors() + 1)
            .setWorkerThreads(Runtime.getRuntime().availableProcessors() + 1)
    );

    /** PG client: */
    PgClient client = new PgClient(getConfig().getConfig("db"));

    /** Template engine: */
    install(new RockerModule());

    /** Single query: */
    get("/db", ctx -> {
      client.selectWorld(Tuple.of(randomWorld()), rsp -> {
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
    }).setNonBlocking(true);

    /** Multiple queries: */
    get("/queries", ctx -> {
      int queries = Util.queries(ctx);
      var result = new ArrayList<World>(queries);
      client.selectWorlds(queries, rsp -> {
        if (rsp.succeeded()) {
          RowIterator<Row> rs = rsp.result().iterator();
          Row row = rs.next();
          result.add(new World(row.getInteger(0), row.getInteger(1)));
        } else {
          sendError(ctx, rsp.cause());
        }
        // ready?
        if (result.size() == queries) {
          ctx.setResponseType(JSON)
              .send(Json.encode(result));
        }
      });
      return ctx;
    }).setNonBlocking(true);

    /** Update queries: */
    get("/updates", ctx -> {
      int queries = Util.queries(ctx);
      World[] result = new World[queries];
      client.selectWorldForUpdate(queries, (index, statement) -> {
        int id = randomWorld();
        statement.execute(Tuple.of(id), selectCallback -> {
          if (selectCallback.failed()) {
            sendError(ctx, selectCallback.cause());
            return;
          }
          result[index] = new World(
              selectCallback.result().iterator().next().getInteger(0),
              randomWorld());
          if (index == queries - 1) {
            client.updateWorld(result, updateCallback -> {
              if (updateCallback.failed()) {
                sendError(ctx, updateCallback.cause());
              } else {
                ctx.setResponseType(JSON)
                    .send(Json.encode(result));
              }
            });
          }
        });
      });
      return ctx;
    }).setNonBlocking(true);

    /** Fortunes: */
    var factory = BufferRockerOutput.factory();
    get("/fortunes", ctx -> {
      client.fortunes(rsp -> {
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
          sendError(ctx, rsp.cause());
        }
      });
      return ctx;
    }).setNonBlocking(true);
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
