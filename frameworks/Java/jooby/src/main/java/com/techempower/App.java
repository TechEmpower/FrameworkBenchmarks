package com.techempower;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.requery.EntityStore;
import io.requery.Persistable;

import org.jooby.Jooby;
import org.jooby.MediaType;

import static org.jooby.MediaType.*;
import org.jooby.Result;
import org.jooby.Results;
import org.jooby.jdbc.Jdbc;
import org.jooby.json.Jackson;
import org.jooby.requery.Requery;
import org.jooby.rocker.Rockerby;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author jooby generator
 */
@SuppressWarnings("unchecked")
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
    /** templates via rocker. */
    use(new Rockerby());

    /** json via jackson . */
    ObjectMapper mapper = new ObjectMapper();
    use(new Jackson(mapper));

    /** database via requery. */
    use(new Jdbc());
    use(new Requery(Models.DEFAULT));

    get("/plaintext", () -> result(HELLO_WORLD, text))
        .renderer("text");

    get("/json", () -> result(mapper.createObjectNode().put("message", HELLO_WORLD), json))
        .renderer("json");

    get("/db", req -> {
      int id = ThreadLocalRandom.current().nextInt(DB_ROWS) + 1;
      EntityStore<Persistable, World> store = require(EntityStore.class);
      World world = store.select(World.class)
          .where(World.ID.eq(id))
          .get()
          .first();
      return result(world, json);
    }).renderer("json");

    get("/fortunes", req -> {
      EntityStore<Persistable, Fortune> store = require(EntityStore.class);
      List<Fortune> fortunes = store.select(Fortune.class) 
          .get()
          .collect(new LinkedList<>());
      Fortune fortune0 = new Fortune();
      fortune0.setMessage("Additional fortune added at request time.");
      fortune0.setId(0);
      fortunes.add(fortune0);
      Collections.sort(fortunes);
      return views.fortunes.template(fortunes);
    });
  }

  private Result result(final Object value, final MediaType type) {
    return Results.ok(value)
        .type(type)
        .header(H_SERVER, SERVER)
        .header(H_DATE, fmt.format(Instant.ofEpochMilli(System.currentTimeMillis())));
  }

  public static void main(final String[] args) {
    run(App::new, args);
  }

}
