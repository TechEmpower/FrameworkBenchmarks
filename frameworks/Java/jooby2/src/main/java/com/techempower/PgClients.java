package com.techempower;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPoolOptions;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

public class PgClients {
  private final Iterator<PgClient> iterator;

  private PgClients(Collection<PgClient> clients) {
    iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
  }

  public synchronized PgClient next() {
    return iterator.next();
  }

  public static PgClients create(PgPoolOptions options) {
    List<PgClient> clients = new ArrayList<>();
    VertxOptions vertxOptions = new VertxOptions();
    vertxOptions.setPreferNativeTransport(true);
    Vertx vertx = Vertx.vertx(vertxOptions);
    for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
      clients.add(PgClient.pool(vertx, options));
    }
    return new PgClients(clients);
  }
}
