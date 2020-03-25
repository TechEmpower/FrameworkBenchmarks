package models;

import java.util.ArrayList;
import java.util.List;

import io.reactiverse.reactivex.pgclient.PgClient;
import io.reactiverse.reactivex.pgclient.PgIterator;
import io.reactiverse.reactivex.pgclient.Row;
import io.reactiverse.reactivex.pgclient.Tuple;
import io.reactivex.Observable;
import module.PgClients;
import ratpack.exec.Promise;
import ratpack.rx2.RxRatpack;

public class PgClientRepository implements DbRepository {
    private final PgClients pgClients;

    public PgClientRepository(PgClients pgClients) {
        this.pgClients = pgClients;
    }

    @Override
    public Promise<World> getWorld(int id) {
        return getWorlds(new int[] { id }).map(worlds -> worlds.get(0));
    }

    @Override
    public Promise<List<World>> getWorlds(int[] ids) {

        PgClient pgClient = pgClients.getOne();

        Observable<World> observable = Observable.range(0, ids.length)
                .flatMap(i -> pgClient.rxPreparedQuery("SELECT * FROM world WHERE id = $1", Tuple.of(ids[i]))
                        .map(rowset -> {
                            final Row row = rowset.iterator().next();

                            return new World(row.getInteger(0), row.getInteger(1));
                        })
                        .toObservable());

        return RxRatpack.promiseAll(observable);
    }

    @Override
    public Promise<List<World>> findAndUpdateWorlds(int[] ids, int[] randomNumbers) {
        return getWorlds(ids).flatMap(worlds -> {
            PgClient pgClient = pgClients.getOne();

            Observable<World> observable = Observable.range(0, worlds.size())
                    .flatMap(i -> {
                        World world = worlds.get(i);
                        world.randomNumber = randomNumbers[i];
                        return pgClient
                                .rxPreparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2", Tuple.of(world.randomNumber, world.id))
                                .map(rowset -> world)
                                .toObservable();
                    });

            return RxRatpack.promiseAll(observable);
        });
    }

    @Override
    public Promise<List<Fortune>> fortunes() {
        return RxRatpack.promiseAll(pgClients.getOne().rxPreparedQuery("SELECT * FROM fortune").flatMapObservable(pgRowSet -> {
            PgIterator resultSet = pgRowSet.iterator();
            List<Fortune> fortunes = new ArrayList<>(pgRowSet.size());
            while (resultSet.hasNext()) {
                Tuple row = resultSet.next();
                fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
            }

            return Observable.fromIterable(fortunes);
        }));
    }
}
