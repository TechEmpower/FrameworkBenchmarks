package com.techempower;

import org.jooby.Jooby;
import org.jooby.MediaType;
import org.jooby.Results;
import org.jooby.Result;
import org.jooby.json.Jackson;
import java.util.Map;
import java.util.HashMap;
import java.util.Locale;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import org.jooby.jdbi.Jdbi;
import org.skife.jdbi.v2.Handle;
import java.util.Random;

/**
 * @author jooby generator
 */
public class App extends Jooby {

  static final DateTimeFormatter fmt = DateTimeFormatter
      .ofPattern("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
      .withZone(ZoneId.of("GMT"));

  static final String H_SERVER = "Server";
  static final String SERVER = "Netty";

  static final String H_DATE= "Date";

  static final String helloWorld = "Hello, World!";

  static final int DB_ROWS = 10000;

  {
    use(new Jackson());
    use(new Jdbi());

    get("*", (req, rsp) -> {
      rsp.header(H_SERVER, SERVER)
         .header(H_DATE, fmt.format(Instant.ofEpochMilli(System.currentTimeMillis())));
    });

    /**
     * Plain text response. Please note all these lines can be just:
     *
     *  get("/plaintext", () -> "Hello, World!");
     *
     *  This way we will get just a few extra ms.
     */
    byte[] plaintextBytes = helloWorld.getBytes(StandardCharsets.UTF_8);
    Result plaintext = Results
        .with(plaintextBytes)
        .type(MediaType.plain);

    get("/plaintext", () -> plaintext);

    /**
     * json response.
     */
    Map<String, Object> hash = new HashMap<>();
    hash.put("message", helloWorld);
    Result json = Results
        .with(hash)
        .type(MediaType.json);

    get("/json", () -> json);

    get("/db", req -> {
      try (Handle handle = req.require(Handle.class)) {
        Random rnd = new Random();
        int id = rnd.nextInt(DB_ROWS);
        return handle.createQuery("select * from World where id = :id")
          .bind("id", id)
          .map((idx, rs, ctx) -> new World(rs.getInt("id"), rs.getInt("randomNumber")))
          .first();
      }
  });
  }

  public static void main(final String[] args) throws Exception {
    new App().start(args);
  }

}
