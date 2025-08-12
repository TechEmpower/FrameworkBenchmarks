package com.techempower;

import static com.techempower.Util.boxedRandomWorld;
import static com.techempower.Util.randomWorld;
import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;

import javax.sql.DataSource;

import io.jooby.Jooby;
import io.jooby.hikari.HikariModule;
import io.jooby.rocker.RockerModule;

public class App extends Jooby {

  private static final String SELECT_WORLD = "select * from world where id=?";

  private static final String MESSAGE = "Hello, World!";

  private static final byte[] MESSAGE_BYTES = MESSAGE.getBytes(StandardCharsets.US_ASCII);

  {
    /** Database: */
    install(new HikariModule());
    DataSource ds = require(DataSource.class);

    /** Template engine: */
    install(new RockerModule());

    var outputFactory = getOutputFactory();
    Json.configure(outputFactory);
    var message = outputFactory.wrap(MESSAGE_BYTES);
    get("/plaintext", ctx ->
        ctx.send(message)
    );

    get("/json", ctx -> ctx
        .setResponseType(JSON)
        .send(Json.encode(new Message(MESSAGE)))
    );

    /** Go blocking: */
    dispatch(() -> {

      /** Single query: */
      get("/db", ctx -> {
        World result;
        try (Connection conn = ds.getConnection()) {
          try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
            statement.setInt(1, randomWorld());
            try (ResultSet rs = statement.executeQuery()) {
              rs.next();
              result = new World(rs.getInt("id"), rs.getInt("randomNumber"));
            }
          }
        }
        return ctx
            .setResponseType(JSON)
            .send(Json.encode(result));
      });

      /** Multiple queries: */
      get("/queries", ctx -> {
        World[] result = new World[Util.queries(ctx)];
        try (Connection conn = ds.getConnection()) {
          for (int i = 0; i < result.length; i++) {
            try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
              statement.setInt(1, randomWorld());
              try (ResultSet rs = statement.executeQuery()) {
                rs.next();
                result[i] = new World(rs.getInt("id"), rs.getInt("randomNumber"));
              }
            }
          }
        }
        return ctx
            .setResponseType(JSON)
            .send(Json.encode(result));
      });

      /** Updates: */
      get("/updates", ctx -> {
        World[] result = new World[Util.queries(ctx)];
        StringJoiner updateSql = new StringJoiner(
            ", ",
            "UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ",
            " ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

        try (Connection connection = ds.getConnection()) {
          try (PreparedStatement statement = connection.prepareStatement(SELECT_WORLD)) {
            for (int i = 0; i < result.length; i++) {
              statement.setInt(1, randomWorld());
              try (ResultSet rs = statement.executeQuery()) {
                rs.next();
                result[i] = new World(rs.getInt("id"), boxedRandomWorld());
              }
              // prepare update query
              updateSql.add("(?, ?)");
            }
          }

          try (PreparedStatement statement = connection.prepareStatement(updateSql.toString())) {
            int i = 0;
            for (World world : result) {
              statement.setInt(++i, world.getId());
              statement.setInt(++i, world.getRandomNumber());
            }
            statement.executeUpdate();
          }
        }
        return ctx.setResponseType(JSON)
            .send(Json.encode(result));
      });

      /** Fortunes: */
      get("/fortunes", ctx -> {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = ds.getConnection()) {
          try (PreparedStatement stt = connection.prepareStatement("select * from fortune")) {
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
        return views.fortunes.template(fortunes);
      });
    });
  }

  public static void main(final String[] args) {
    runApp(args, EVENT_LOOP, App::new);
  }
}
