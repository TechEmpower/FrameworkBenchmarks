package com.techempower;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;
import io.vertx.core.Vertx;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

public class PgClients {
  private final Iterator<PgPool> iterator;

  private PgClients(Collection<PgPool> clients) {
    iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
  }

  public synchronized PgPool next() {
    return iterator.next();
  }

  public static PgClients create(Vertx vertx, PgPoolOptions options) {
    List<PgPool> clients = new ArrayList<>();
    for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
      clients.add(PgClient.pool(vertx, options));
    }
    return new PgClients(clients);
  }
}
