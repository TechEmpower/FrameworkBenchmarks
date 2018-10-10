package models;

import io.reactiverse.rxjava.pgclient.PgClient;
import io.reactiverse.rxjava.pgclient.PgIterator;
import io.reactiverse.rxjava.pgclient.Row;
import io.reactiverse.rxjava.pgclient.Tuple;
import module.PgClients;
import ratpack.exec.Promise;
import ratpack.rx.RxRatpack;
import rx.Observable;

import java.util.ArrayList;
import java.util.List;

public class PgClientRepository implements DbRepository {
    private final PgClients pgClients;

    public PgClientRepository(PgClients pgClients) {
        this.pgClients = pgClients;
    }

    @Override
    public Promise<World> getWorld(int id) {
        return getWorlds(new int[]{id}).map(worlds -> worlds.get(0));
    }

    @Override
    public Promise<List<World>> getWorlds(int[] ids) {

        Observable<World>[] observables = new Observable[ids.length];

        PgClient pgClient = pgClients.getOne();

        for (int i = 0; i < ids.length; i++) {
            observables[i] = pgClient.rxPreparedQuery("SELECT * FROM world WHERE id = $1", Tuple.of(ids[i])).map(rowset -> {
                final Row row = rowset.iterator().next();

                return new World(row.getInteger(0), row.getInteger(1));
            }).toObservable();
        }

        return getPromise(observables);
    }

    @Override
    public Promise<List<World>> findAndUpdateWorlds(int[] ids, int[] randomNumbers) {
        return getWorlds(ids).flatMap(worlds -> {
            Observable<World>[] observables = new Observable[worlds.size()];

            PgClient pgClient = pgClients.getOne();

            for (int i = 0; i < worlds.size(); i++) {
                World world = worlds.get(i);
                world.randomNumber = randomNumbers[i];
                observables[i] = pgClient.rxPreparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2", Tuple.of(world.randomNumber, world.id)).map(rowset -> world).toObservable();
            }

            return getPromise(observables);
        });
    }

    private Promise<List<World>> getPromise(Observable<World>[] observables) {
        return RxRatpack.promiseSingle(
                Observable.merge(observables)
                        .collect(() -> new ArrayList<World>(), (worlds, world) -> worlds.add(world)));
    }

    @Override
    public Promise<List<Fortune>> fortunes() {
        return RxRatpack.promise(pgClients.getOne().rxPreparedQuery("SELECT * FROM fortune").flatMapObservable(pgRowSet -> {
            PgIterator resultSet = pgRowSet.iterator();
            List<Fortune> fortunes = new ArrayList<>(pgRowSet.size());
            while (resultSet.hasNext()) {
                Tuple row = resultSet.next();
                fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
            }

            return Observable.from(fortunes);
        }));
    }
}
