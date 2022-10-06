package benchmark.repository;

import benchmark.model.World;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;

import java.util.Collection;
import java.util.List;

public interface ReactiveWorldRepository {

    Publisher<Void> initDb(Collection<World> worlds);

    Publisher<World> findById(int id);

    default Publisher<List<World>> findByIds(List<Integer> ids) {
        return Flux.fromIterable(ids).flatMap(this::findById).collectList();
    }

    Publisher<Void> updateAll(Collection<World> worlds);

}
