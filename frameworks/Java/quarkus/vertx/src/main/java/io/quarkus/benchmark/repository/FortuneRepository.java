package io.quarkus.benchmark.repository;

import io.quarkus.benchmark.model.Fortune;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Singleton
public class FortuneRepository {

    @Inject
    PgConnectionPool pgConnectionPool;

    public void findAllSortedFortunes(final Handler<AsyncResult<List<Fortune>>> resultHandler) {
        pgConnectionPool.pgConnection().selectFortuneQuery()
                .execute(fortuneRows -> {
                    if (fortuneRows.succeeded()) {
                        final List<Fortune> fortunes = new ArrayList<>(fortuneRows.result().size() + 1);
                        final RowIterator<Row> resultSet = fortuneRows.result().iterator();
                        if (!resultSet.hasNext()) {
                            resultHandler.handle(Future.succeededFuture(List.of()));
                            return;
                        }
                        while (resultSet.hasNext()) {
                            final Row row = resultSet.next();
                            fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
                        }
                        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                        Collections.sort(fortunes);
                        resultHandler.handle(Future.succeededFuture(fortunes));
                    } else {
                        resultHandler.handle(Future.failedFuture(fortuneRows.cause()));
                    }
                });
    }
}
