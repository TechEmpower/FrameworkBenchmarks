package benchmark.repository;

import benchmark.model.Fortune;
import org.reactivestreams.Publisher;

import java.util.Collection;
import java.util.List;

public interface ReactiveFortuneRepository {

    Publisher<Void> initDb(Collection<Fortune> fortunes);

    Publisher<List<Fortune>> findAll();

}
