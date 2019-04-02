package com.techempower;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.jooby.Context;
import io.jooby.Env;
import io.jooby.Jooby;
import io.jooby.hikari.Hikari;
import io.jooby.json.Jackson;

import java.io.OutputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.html;
import static io.jooby.MediaType.json;
import static java.sql.ResultSet.CONCUR_READ_ONLY;
import static java.sql.ResultSet.TYPE_FORWARD_ONLY;

public class App extends Jooby {

  private static final String UPDATE_WORLD = "update world set randomNumber=? where id=?";

  private static final String SELECT_WORLD = "select * from world where id=?";

  private static final int DB_ROWS = 10000;

  private static final String HELLO_WORLD = "Hello, World!";

  public static class Message {
    public final String message = HELLO_WORLD;
  }

  {
    Env env = Env.defaultEnvironment(getClass().getClassLoader());

    /** JSON: */
    ObjectMapper mapper = Jackson.defaultObjectMapper();

    /** Database: */
    HikariConfig conf = new Hikari.Builder().build(env);
    DataSource ds = new HikariDataSource(conf);

    get("/plaintext", ctx ->
        ctx.sendString(HELLO_WORLD)
    );

    get("/json", ctx -> ctx
        .setContentType(json)
        .sendBytes(mapper.writeValueAsBytes(new Message()))
    );

    /** Go blocking: */
    dispatch(() -> {

      /** Single query: */
      get("/db", ctx -> ctx
          .setContentType(json)
          .sendBytes(mapper.writeValueAsBytes(findOne(ds, rndId(ThreadLocalRandom.current()))))
      );

      /** Multiple queries: */
      get("/queries", ctx -> {
        // bound count
        int count = queries(ctx);
        List<World> result = new ArrayList<>();
        Random rnd = ThreadLocalRandom.current();
        try (Connection conn = ds.getConnection()) {
          for (int i = 0; i < count; i++) {
            int id = rndId(rnd);
            try (final PreparedStatement query = conn
                .prepareStatement(SELECT_WORLD, TYPE_FORWARD_ONLY,
                    CONCUR_READ_ONLY)) {
              result.add(findOne(query, id));
            }
          }
        }
        ctx.setContentType(json);
        return ctx.sendBytes(mapper.writeValueAsBytes(result));
      });

      /** Updates: */
      get("/updates", ctx -> {
        // bound count
        int count = queries(ctx);
        List<World> result = new ArrayList<>();
        Random rnd = ThreadLocalRandom.current();
        try (Connection conn = ds.getConnection()) {
          for (int i = 0; i < count; i++) {
            int id = rndId(rnd);
            int newRandomNumber = rndId(rnd);
            try (final PreparedStatement query = conn
                .prepareStatement(SELECT_WORLD, TYPE_FORWARD_ONLY, CONCUR_READ_ONLY);
                final PreparedStatement update = conn.prepareStatement(UPDATE_WORLD)) {
              // find
              World world = findOne(query, id);

              // update
              update.setInt(1, newRandomNumber);
              update.setInt(2, id);
              update.execute();

              result.add(new World(world.id, newRandomNumber));
            }
          }
        }
        ctx.setContentType(json);
        return ctx.sendBytes(mapper.writeValueAsBytes(result));
      });

      /** Fortunes: */
      get("/fortunes", ctx -> {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = ds.getConnection()) {
          try (PreparedStatement stt = connection
              .prepareStatement("select * from fortune", TYPE_FORWARD_ONLY, CONCUR_READ_ONLY)) {
            try (ResultSet rs = stt.executeQuery()) {
              while (rs.next()) {
                fortunes.add(new Fortune(rs.getInt("id"), rs.getString("message")));
              }
            }
          }
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);

        /** render view: */
        views.fortunes template = views.fortunes.template(fortunes);
        ArrayOfByteArraysOutput buff = template.render(ArrayOfByteArraysOutput.FACTORY);
        ctx.setContentLength(buff.getByteLength());
        try (OutputStream output = ctx.responseStream(html)) {
          for (byte[] chunk : buff.getArrays()) {
            output.write(chunk);
          }
        }
        return ctx;
      });
    });
  }

  public static void main(final String[] args) {
    run(App::new, EVENT_LOOP, args);
  }

  private World findOne(DataSource ds, int id) throws SQLException {
    try (Connection conn = ds.getConnection()) {
      try (PreparedStatement query = conn
          .prepareStatement(SELECT_WORLD, TYPE_FORWARD_ONLY,
              CONCUR_READ_ONLY)) {
        return findOne(query, id);
      }
    }
  }

  private World findOne(PreparedStatement query, int id) throws SQLException {
    query.setInt(1, id);
    try (ResultSet resultSet = query.executeQuery()) {
      resultSet.next();
      return new World(resultSet.getInt("id"), resultSet.getInt("randomNumber"));
    }
  }

  private final int rndId(Random rnd) {
    return rnd.nextInt(DB_ROWS) + 1;
  }

  private int   queries(Context ctx) {
    String value = ctx.query("queries").value("");
    if (value.length() == 0) {
      return 1;
    }
    try {
      return Math.min(500, Math.max(1, Integer.parseInt(value)));
    } catch (NumberFormatException x) {
      return 1;
    }
  }
}
