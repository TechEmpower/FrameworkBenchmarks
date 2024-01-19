package io.quarkus.benchmark.repository;

import java.util.ArrayList;
import java.util.List;

import io.quarkus.benchmark.model.Fortune;
import io.smallrye.mutiny.Uni;
import io.vertx.mutiny.sqlclient.Row;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

@Singleton
public class FortuneRepository {

    @Inject
    PgClients clients;

    public Uni<List<Fortune>> findAll() {
        return clients.getClient().preparedQuery("SELECT * FROM Fortune")
                .execute()
                .map(rowset -> {
                    final List<Fortune> ret = new ArrayList<>(rowset.size() + 1);
                    for (final Row r : rowset) {
                        ret.add(new Fortune(r.getInteger(0), r.getString(1)));
                    }
                    return ret;
                });
    }
}