package benchmark.repository;

import io.reactiverse.pgclient.PgClient;

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Stream;

class PgClients {
    private final Iterator<PgClient> iterator;

    PgClients(Collection<PgClient> clients) {
        iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
    }

    synchronized PgClient getOne() {
        return iterator.next();
    }
}