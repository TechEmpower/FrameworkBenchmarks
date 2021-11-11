package com.techempower;

import io.jooby.Context;
import io.jooby.annotations.Dispatch;
import views.fortunes;

import javax.sql.DataSource;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;

import static com.techempower.Util.randomWorld;
import static io.jooby.MediaType.JSON;

public class Resource {
  private static final String SELECT_WORLD = "select * from world where id=?";

  private static final String MESSAGE = "Hello, World!";

  private static final byte[] MESSAGE_BYTES = MESSAGE.getBytes(StandardCharsets.UTF_8);

  private final DataSource dataSource;

  public Resource(DataSource dataSource) {
    this.dataSource = dataSource;
  }

  @GET @Path("/plaintext")
  public void plaintText(Context ctx) {
    ctx.send(MESSAGE_BYTES);
  }

  @GET @Path("/json")
  public void json(Context ctx) throws IOException {
    ctx.setResponseType(JSON);
    ctx.send(Json.encode(new Message(MESSAGE)));
  }

  @GET @Path("/db")
  @Dispatch
  public void db(Context ctx) throws Exception {
    World result;
    try (Connection conn = dataSource.getConnection()) {
      try (final PreparedStatement statement = conn.prepareStatement(SELECT_WORLD)) {
        statement.setInt(1, randomWorld());
        try (ResultSet rs = statement.executeQuery()) {
          rs.next();
          result = new World(rs.getInt("id"), rs.getInt("randomNumber"));
        }
      }
    }
    ctx.setResponseType(JSON);
    ctx.send(Json.encode(result));
  }

  @GET @Path("/queries")
  @Dispatch
  public void queries(Context ctx) throws Exception {
    World[] result = new World[Util.queries(ctx)];
    try (Connection conn = dataSource.getConnection()) {
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
    ctx.setResponseType(JSON);
    ctx.send(Json.encode(result));
  }

  @GET @Path("/updates")
  @Dispatch
  public void updates(Context ctx) throws Exception {
    World[] result = new World[Util.queries(ctx)];
    StringJoiner updateSql = new StringJoiner(
        ", ",
        "UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ",
        " ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

    try (Connection connection = dataSource.getConnection()) {
      try (PreparedStatement statement = connection.prepareStatement(SELECT_WORLD)) {
        for (int i = 0; i < result.length; i++) {
          statement.setInt(1, randomWorld());
          try (ResultSet rs = statement.executeQuery()) {
            rs.next();
            result[i] = new World(rs.getInt("id"), randomWorld());
          }
          // prepare update query
          updateSql.add("(?, ?)");
        }
      }

      try (PreparedStatement statement = connection.prepareStatement(updateSql.toString())) {
        int i = 0;
        for (World world : result) {
          statement.setInt(++i, world.getRandomNumber());
          statement.setInt(++i, world.getRandomNumber());
        }
        statement.executeUpdate();
      }
    }
    ctx.setResponseType(JSON);
    ctx.send(Json.encode(result));
  }

  @GET @Path("/fortunes")
  @Dispatch
  public fortunes fortunes(Context ctx) throws Exception {
    List<Fortune> fortunes = new ArrayList<>();
    try (Connection connection = dataSource.getConnection()) {
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
  }
}
