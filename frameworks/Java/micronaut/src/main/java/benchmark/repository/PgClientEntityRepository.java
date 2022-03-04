package benchmark.repository;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.vertx.reactivex.pgclient.PgPool;
import io.vertx.reactivex.sqlclient.Row;
import io.vertx.reactivex.sqlclient.Tuple;
import jakarta.inject.Singleton;

@Singleton
public class PgClientEntityRepository implements EntityRepository {
    private final PgPool pgClient;

    public PgClientEntityRepository(PgPool pgClient) {
        this.pgClient = pgClient;
    }

    @Override
    public Single<World> getWorld(int id) {
        return Single.create(sink ->
                pgClient.preparedQuery("SELECT * FROM world WHERE id = $1").execute(Tuple.of(id), ar -> {
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
        return Single.create(sink -> pgClient.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                .execute(Tuple.of(world.getRandomNumber(), world.getId()), ar -> {
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
            world.setRandomNumber(randomNumber);
            return updateWorld(world);
        });
    }

    @Override
    public Flowable<Fortune> fortunes() {
        return Flowable.create(sink ->
                pgClient.preparedQuery("SELECT * FROM fortune").execute(ar -> {
                    if (ar.failed()) {
                        sink.onError(ar.cause());
                        return;
                    }

                    for (Row row : ar.result()) {
                        sink.onNext(new Fortune(row.getInteger(0), row.getString(1)));
                    }
                    sink.onComplete();
                }), BackpressureStrategy.BUFFER);
    }
}
