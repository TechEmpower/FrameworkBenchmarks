package benchmark.repository;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import io.reactivex.Flowable;
import io.reactivex.Single;

public interface DbRepository {
    Single<World> getWorld(int id);

    Single<World> findAndUpdateWorld(int id, int randomNumber);

    Flowable<Fortune> fortunes();
}