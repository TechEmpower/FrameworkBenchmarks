package com.techempower;

import static com.techempower.Util.boxedRandomWorld;
import static com.techempower.Util.randomWorld;
import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;

import java.util.*;

import io.jooby.*;
import io.jooby.rocker.RockerModule;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.Tuple;

public class ReactivePg extends Jooby {
  {
    /** PG client: */
    PgClient client = new PgClient(getConfig().getConfig("db"));

    /** Template engine: */
    install(new RockerModule());
    Json.configure(getOutputFactory());

    /** Single query: */
    get("/db", ctx -> {
      client.selectWorld(Tuple.of(boxedRandomWorld()), rsp -> {
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
    });

    /** Update queries: */
    get("/updates", ctx -> {
      int queries = Util.queries(ctx);
      World[] result = new World[queries];
      var updater = client.updater();
      updater.selectWorldForUpdate(queries, (index, statement) -> {
        statement.execute(Tuple.of(boxedRandomWorld())).onComplete(rsp -> {
          if (rsp.failed()) {
            sendError(ctx, rsp.cause());
            return;
          }
          result[index] = new World(
              rsp.result().iterator().next().getInteger(0),
              boxedRandomWorld());
          if (index == queries - 1) {
            updater.updateWorld(result, updateCallback -> {
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
    });

    /** Fortunes: */
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
          ctx.setResponseType(MediaType.html)
                  .render(views.fortunes.template(fortunes));
        } else {
          sendError(ctx, rsp.cause());
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
