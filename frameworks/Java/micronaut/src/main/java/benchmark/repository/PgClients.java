package benchmark.repository;

import io.vertx.reactivex.pgclient.PgPool;

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Stream;

class PgClients {
    private final Iterator<PgPool> iterator;

    PgClients(Collection<PgPool> clients) {
        iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
    }

    synchronized PgPool getOne() {
        return iterator.next();
    }
}
