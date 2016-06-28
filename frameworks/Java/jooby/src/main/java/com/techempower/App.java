package com.techempower;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.concurrent.ThreadLocalRandom;

import org.jooby.Jooby;
import org.jooby.MediaType;
import org.jooby.Result;
import org.jooby.Results;
import org.jooby.jdbi.Jdbi;
import org.jooby.json.Jackson;
import org.skife.jdbi.v2.Handle;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @author jooby generator
 */
public class App extends Jooby {

  static final DateTimeFormatter fmt = DateTimeFormatter
      .ofPattern("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
      .withZone(ZoneId.of("GMT"));

  static final String H_SERVER = "Server";
  static final String SERVER = "Netty";

  static final String H_DATE = "Date";

  static final int DB_ROWS = 10000;

  static final String HELLO_WORLD = "Hello, World!";

  {
    /** json via jackson .*/
    ObjectMapper mapper = new ObjectMapper();
    use(new Jackson(mapper));

    /** database via jdbi .*/
    use(new Jdbi());

    get("/plaintext", () -> result(HELLO_WORLD, MediaType.text))
        .renderer("text");

    get("/json", () -> result(mapper.createObjectNode().put("message", HELLO_WORLD), MediaType.json))
        .renderer("json");

    get("/db", req -> {
      try (Handle handle = req.require(Handle.class)) {
        int id = ThreadLocalRandom.current().nextInt(DB_ROWS + 1);
        return result(
            handle.createQuery("select * from World where id = :id")
                .bind("id", id)
                .map((idx, rs, ctx) -> new World(rs.getInt("id"), rs.getInt("randomNumber")))
                .first(),
            MediaType.json);
      }
    }).renderer("json");

  }

  private Result result(final Object value, final MediaType type) {
    return Results.ok(value).type(type)
        .header(H_SERVER, SERVER)
        .header(H_DATE, fmt.format(Instant.ofEpochMilli(System.currentTimeMillis())));
  }

  public static void main(final String[] args) throws Exception {
    new App().start(args);
  }

}
