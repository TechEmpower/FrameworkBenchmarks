package module;

import io.reactiverse.reactivex.pgclient.PgClient;

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Stream;

public class PgClients {
    private final Iterator<PgClient> iterator;

    public PgClients(Collection<PgClient> clients) {
        this.iterator = Stream.generate(() -> clients).flatMap(Collection::stream).iterator();
    }

    public synchronized PgClient getOne() {
        return iterator.next();
    }
}
