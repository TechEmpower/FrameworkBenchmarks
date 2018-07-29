package benchmark;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.reactiverse.reactivex.pgclient.*;
import io.reactivex.Flowable;
import io.reactivex.Single;

import javax.annotation.Nullable;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

@Controller("/")
public class Database {

    private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";
    private final Mustache mustache;
    private final PgPool pgPool;

    public Database(PgPool pgPool) {
        this.pgPool = pgPool;
        this.mustache = new DefaultMustacheFactory().compile("fortunes.mustache");
    }

    @Get("/db")
    public Single<World> db() {
        return pgPool.rxGetConnection()
                .doAfterSuccess(PgConnection::close)
                .flatMap((connection) -> {
                    return findRandomWorld(connection.rxPrepare(SELECT_WORLD), null);
                });
    }

    @Get("/queries")
    public Flowable<World> queries(@QueryValue String queries) {
        return findRandomWorlds(parseQueryCount(queries));
    }

    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    public Writer fortune() {
        return mustache.execute(new StringWriter(), findFortunes());
    }

    @Get("/updates")
    public Single<List<World>> updates(@QueryValue String queries) {
        return updateRandomWorlds(parseQueryCount(queries));
    }

    private Single<World> findRandomWorld(Single<PgPreparedQuery> preparedQuery, @Nullable Integer id) {
        return preparedQuery
                .flatMap(query -> query.rxExecute(Tuple.of(id != null ? id : nextNumber())))
                .map((result) -> {
                    Row row = result.iterator().next();
                    return new World(row.getInteger("id"), row.getInteger("randomnumber"));
                });
    }

    private int nextNumber() {
        return ThreadLocalRandom.current().nextInt(10000) + 1;
    }

    private Flowable<World> findRandomWorlds(int count) {
        return pgPool.rxGetConnection()
                .toFlowable()
                .doAfterNext(PgConnection::close)
                .flatMap((connection) -> {
                    return findRandomWorlds(connection.rxPrepare(SELECT_WORLD), count);
                });
    }

    private Flowable<World> findRandomWorlds(Single<PgPreparedQuery> preparedQuery, int count) {
        return Flowable.range(1, count)
                .flatMap(i -> findRandomWorld(preparedQuery, i).toFlowable());
    }

    private Single<List<World>> updateRandomWorlds(int count) {
        return pgPool.rxGetConnection()
                .doAfterSuccess(PgConnection::close)
                .flatMap(connection -> {
                        return findRandomWorlds(connection.rxPrepare(SELECT_WORLD), count)
                                .doOnNext(world -> world.setRandomNumber(nextNumber()))
                                .toList(count)
                                .flatMap(worlds -> {
                                    int worldCount = worlds.size();
                                    List<Tuple> tuples = new ArrayList<>(worldCount);
                                    for (int i = 0; i < worldCount; i++) {
                                        World world = worlds.get(i);
                                        tuples.add(Tuple.of(world.getRandomNumber(), world.getId()));
                                    }
                                    return connection.rxPreparedBatch(UPDATE_WORLD, tuples)
                                            .map(pgRowSet -> worlds);
                                });

                });
    }

    private List<Fortune> findFortunes() {
        return pgPool.rxGetConnection()
                .doAfterSuccess(PgConnection::close)
                .flatMap((connection) -> {
                    return connection.rxPreparedQuery(SELECT_FORTUNE).map((result) -> {
                        PgIterator iterator = result.iterator();
                        List<Fortune> fortunes = new ArrayList<>();
                        while (iterator.hasNext()) {
                            Row row = iterator.next();
                            fortunes.add(new Fortune(row.getInteger("id"), row.getString("message")));
                        }
                        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                        fortunes.sort(Comparator.comparing(Fortune::getMessage));

                        return fortunes;
                    });
                }).blockingGet();
    }

    private int parseQueryCount(String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
