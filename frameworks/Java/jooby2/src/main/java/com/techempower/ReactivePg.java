package com.techempower;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.typesafe.config.Config;
import io.jooby.Jooby;
import io.jooby.json.JacksonModule;
import io.jooby.rocker.RockerModule;
import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static com.techempower.Util.randomWorld;
import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;

public class ReactivePg extends Jooby {

  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

  {
    /** PG client: */
    PgClient client = database(getConfig().getConfig("db"));

    /** Template engine: */
    install(new RockerModule());

    /** JSON: */
    install(new JacksonModule());
    ObjectMapper mapper = require(ObjectMapper.class);

    /** Single query: */
    get("/db", ctx -> {
      client.preparedQuery(SELECT_WORLD, Tuple.of(randomWorld()), rsp -> {
        try {
          if (rsp.succeeded()) {
            PgIterator rs = rsp.result().iterator();
            Row row = rs.next();
            ctx.setResponseType(JSON)
                .send(mapper.writeValueAsBytes(new World(row.getInteger(0), row.getInteger(1))));
          } else {
            ctx.sendError(rsp.cause());
          }
        } catch (IOException x) {
          ctx.sendError(x);
        }
      });
      return ctx;
    });

    /** Multiple queries: */
    get("/queries", ctx -> {
      int queries = Util.queries(ctx);
      AtomicInteger counter = new AtomicInteger();
      AtomicBoolean failed = new AtomicBoolean(false);
      World[] result = new World[queries];
      for (int i = 0; i < result.length; i++) {
        client.preparedQuery(SELECT_WORLD, Tuple.of(randomWorld()), rsp -> {
          if (rsp.succeeded()) {
            PgIterator rs = rsp.result().iterator();
            Row row = rs.next();
            result[counter.get()] = new World(row.getInteger(0), row.getInteger(1));
          } else {
            if (failed.compareAndSet(false, true)) {
              ctx.sendError(rsp.cause());
            }
          }
          // ready?
          if (counter.incrementAndGet() == queries && !failed.get()) {
            try {
              ctx.setResponseType(JSON)
                  .send(mapper.writeValueAsBytes(result));
            } catch (IOException x) {
              ctx.sendError(x);
            }
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
      AtomicBoolean failed = new AtomicBoolean(false);

      for (int i = 0; i < queries; i++) {
        client.preparedQuery(SELECT_WORLD, Tuple.of(randomWorld()), rsp -> {
          if (rsp.succeeded()) {
            PgIterator rs = rsp.result().iterator();
            Tuple row = rs.next();
            World world = new World(row.getInteger(0), row.getInteger(1));
            world.randomNumber = randomWorld();
            result[counter.get()] = world;
          } else {
            if (failed.compareAndSet(false, true)) {
              ctx.sendError(rsp.cause());
            }
          }

          if (counter.incrementAndGet() == queries && !failed.get()) {
            List<Tuple> batch = new ArrayList<>(queries);
            for (World world : result) {
              batch.add(Tuple.of(world.randomNumber, world.id));
            }

            client.preparedBatch(UPDATE_WORLD, batch, ar -> {
              if (ar.failed()) {
                ctx.sendError(ar.cause());
              } else {
                try {
                  ctx.setResponseType(JSON)
                      .send(mapper.writeValueAsBytes(result));
                } catch (IOException x) {
                  ctx.sendError(x);
                }
              }
            });
          }
        });
      }
      return ctx;
    });

    /** Fortunes: */
    get("/fortunes", ctx -> {
      client.preparedQuery(SELECT_FORTUNE, rsp -> {
        if (rsp.succeeded()) {
          PgIterator rs = rsp.result().iterator();

          List<Fortune> fortunes = new ArrayList<>();

          while (rs.hasNext()) {
            Row row = rs.next();
            fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
          }

          fortunes.add(new Fortune(0, "Additional fortune added at request time."));
          Collections.sort(fortunes);

          /** render view: */
          views.fortunes template = views.fortunes.template(fortunes);
          ctx.render(template);
        } else {
          ctx.sendError(rsp.cause());
        }
      });
      return ctx;
    });
  }

  private PgClient database(Config config) {
    PgPoolOptions options = new PgPoolOptions();
    options.setDatabase(config.getString("databaseName"));
    options.setHost(config.getString("serverName"));
    options.setPort(config.getInt("portNumber"));
    options.setUser(config.getString("user"));
    options.setPassword(config.getString("password"));
    options.setCachePreparedStatements(true);
    return PgClient.pool(new PgPoolOptions(options).setMaxSize(4));
  }

  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, ReactivePg::new);
  }
}
