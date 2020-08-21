package io.quarkus.benchmark.repository;

import java.util.ArrayList;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

import io.quarkus.benchmark.model.Fortune;
import io.smallrye.mutiny.Uni;
import io.vertx.mutiny.sqlclient.Row;

@ApplicationScoped
public class FortuneRepository {

    @Inject
    PgClients clients;

    public Uni<List<Fortune>> findAll() {
        return clients.getClient().preparedQuery("SELECT * FROM Fortune" )
                .execute()
                .map(rowset -> {
                    List<Fortune> ret = new ArrayList<>(rowset.size()+1);
                    for(Row r : rowset) {
                        ret.add(new Fortune(r.getInteger("id"), r.getString("message")));
                    }
                    return ret;
                });
    }
}
