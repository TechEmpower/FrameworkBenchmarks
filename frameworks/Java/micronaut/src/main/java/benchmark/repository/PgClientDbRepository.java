package benchmark.repository;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;
import io.reactivex.BackpressureStrategy;
import io.reactivex.Flowable;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Singleton;

@Singleton
public class PgClientDbRepository implements DbRepository {
    private final Logger log = LoggerFactory.getLogger(getClass());
    private final PgClients pgClients;

    public PgClientDbRepository(PgClients pgClients) {
        this.pgClients = pgClients;
    }

    @Override
    public Single<World> getWorld(int id) {
        return Single.create(sink ->
                pgClients.getOne().preparedQuery("SELECT * FROM world WHERE id = $1", Tuple.of(id), ar -> {
                    if (ar.failed()) {
                        sink.onError(ar.cause());
                    } else {

                        final Row row = ar.result().iterator().next();

                        World world = new World(row.getInteger(0), row.getInteger(1));
                        sink.onSuccess(world);
                    }
                }));
    }

    private Single<World> updateWorld(World world) {
        return Single.create(sink -> pgClients.getOne().preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2", Tuple.of(world.randomNumber, world.id), ar -> {
            if (ar.failed()) {
                sink.onError(ar.cause());
            } else {
                sink.onSuccess(world);
            }
        }));
    }

    @Override
    public Single<World> findAndUpdateWorld(int id, int randomNumber) {
        return getWorld(id).flatMap(world -> {
            world.randomNumber = randomNumber;
            return updateWorld(world);
        });
    }

    @Override
    public Flowable<Fortune> fortunes() {
        return Flowable.create(sink ->
                pgClients.getOne().preparedQuery("SELECT * FROM fortune", ar -> {
                    if (ar.failed()) {
                        sink.onError(ar.cause());
                        return;
                    }

                    PgIterator resultSet = ar.result().iterator();
                    while (resultSet.hasNext()) {
                        Tuple row = resultSet.next();
                        sink.onNext(new Fortune(row.getInteger(0), row.getString(1)));
                    }
                    sink.onComplete();
                }), BackpressureStrategy.BUFFER);
    }
}