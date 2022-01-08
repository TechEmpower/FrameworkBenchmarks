package benchmark.repository;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

public interface EntityRepository {
    Single<World> getWorld(int id);

    Single<World> findAndUpdateWorld(int id, int randomNumber);

    Flowable<Fortune> fortunes();
}