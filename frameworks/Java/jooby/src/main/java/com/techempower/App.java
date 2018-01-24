package com.techempower;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.jooby.Jooby;
import org.jooby.MediaType;
import org.jooby.Request;

import static org.jooby.MediaType.*;
import org.jooby.Result;
import org.jooby.Status;
import org.jooby.jdbc.Jdbc;
import org.jooby.json.Jackson;
import org.jooby.rocker.Rockerby;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.inject.Inject;
import javax.sql.DataSource;

/**
 * @author jooby generator
 */
public class App extends Jooby {

  private static final String UPDATE_WORLD = "update world set randomNumber=? where id=?";

  private static final String SELECT_WORLD = "select * from world where id=?";

  static final DateTimeFormatter fmt = DateTimeFormatter
      .ofPattern("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
      .withZone(ZoneId.of("GMT"));

  static final String H_SERVER = "Server";
  static final String SERVER = "Netty";

  static final String H_DATE = "Date";

  static final int DB_ROWS = 10000;

  static final String HELLO_WORLD = "Hello, World!";

  public static class Message {
    public final String message = HELLO_WORLD;
  }

  @Inject
  private DataSource ds;

  {
    /** Template engine: */
    use(new Rockerby());

    /** JSON: */
    ObjectMapper mapper = new ObjectMapper();

    /** Database: */
    use(new Jdbc());

    get("/plaintext", (req, rsp) -> {
      rsp.send(result(HELLO_WORLD, plain));
    }).renderer("text");

    get("/json", (req, rsp) -> {
      rsp.send(result(mapper.writeValueAsString(new Message()), json));
    }).renderer("text");

    /** Single query: */
    get("/db", (req, rsp) -> {
      rsp.send(result(mapper.writeValueAsString(findOne(rndId())), json));
    }).renderer("text");

    /** Multiple queries: */
    get("/queries", (req, rsp) -> {
      // bound count
      int count = queries(req);
      List<World> result = new ArrayList<>();
      Random rnd = ThreadLocalRandom.current();
      try (Connection conn = ds.getConnection()) {
        for (int i = 0; i < count; i++) {
          int id = rndId(rnd);
          try (final PreparedStatement query = conn.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            result.add(findOne(query, id));
          }
        }
      }

      rsp.send(result(mapper.writeValueAsString(result), json));
    }).renderer("text");

    /** Updates: */
    get("/updates", (req, rsp) -> {
      // bound count
      int count = queries(req);
      List<World> result = new ArrayList<>();
      Random rnd = ThreadLocalRandom.current();
      try (Connection conn = ds.getConnection()) {
        for (int i = 0; i < count; i++) {
          int id = rndId(rnd);
          int newRandomNumber = rndId(rnd);
          try (final PreparedStatement query = conn.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
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

      rsp.send(result(mapper.writeValueAsString(result), json));
    }).renderer("text");

    /** Fortunes: */
    get("/fortunes", (req, rsp) -> {
      List<Fortune> fortunes = new ArrayList<>();
      try (Connection connection = ds.getConnection()) {
        try (PreparedStatement stt = connection.prepareStatement("select * from fortune", ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
          try (ResultSet rs = stt.executeQuery()) {
            while (rs.next()) {
              fortunes.add(new Fortune(rs.getInt("id"), rs.getString("message")));
            }
          }
        }
      }
      fortunes.add(new Fortune(0, "Additional fortune added at request time."));
      Collections.sort(fortunes);
      rsp.send(result(views.fortunes.template(fortunes), html));
    });
  }

  public static void main(final String[] args) {
    run(App::new, args);
  }

  private World findOne(int id) throws SQLException {
    try (Connection conn = ds.getConnection()) {
      try (PreparedStatement query = conn.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
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

  private final int rndId() {
    return rndId(ThreadLocalRandom.current());
  }

  private final int rndId(Random rnd) {
    return rnd.nextInt(DB_ROWS) + 1;
  }

  private Result result(final Object value, final MediaType type) {
    return new Result().set(value)
        .status(Status.OK)
        .type(type)
        .header(H_SERVER, SERVER)
        .header(H_DATE, fmt.format(Instant.ofEpochMilli(System.currentTimeMillis())));
  }

  private int queries(Request req) {
    String value = req.param("queries").value("");
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
