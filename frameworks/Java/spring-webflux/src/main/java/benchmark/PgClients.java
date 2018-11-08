package benchmark;

import io.reactiverse.pgclient.PgClient;

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Stream;

public class PgClients {
    private final Iterator<PgClient> iterator;

    public PgClients(Collection<PgClient> clients) {
        iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
    }

    public synchronized PgClient getOne() {
        return iterator.next();
    }
}