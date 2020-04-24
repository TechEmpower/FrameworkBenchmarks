package io.quarkus.benchmark.repository.pgclient;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

import io.quarkus.benchmark.model.Fortune;
import io.reactivex.Scheduler;
import io.reactivex.Single;
import io.reactivex.schedulers.Schedulers;
import io.vertx.reactivex.pgclient.PgPool;
import io.vertx.reactivex.sqlclient.Row;
import io.vertx.reactivex.sqlclient.RowSet;

@ApplicationScoped
public class FortuneRepository {

    @Inject
    PgPool client;

    private final Scheduler scheduler = Schedulers.from(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 2));

    public Single<List<Fortune>> findAll() {
        return client.rxQuery("SELECT id, message FROM fortune")
                .map(RowSet::iterator)
                .map(rowIterator -> {
                    List<Fortune> fortunes = new ArrayList<>();
                    while (rowIterator.hasNext()) {
                        Row row = rowIterator.next();
                        Fortune fortune = new Fortune(row.getInteger(0), row.getString(1));
                        fortunes.add(fortune);
                    }
                    return fortunes;
                })
                .subscribeOn(scheduler);
    }
}
