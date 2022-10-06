package benchmark.repository;

import benchmark.model.Fortune;
import org.reactivestreams.Publisher;

import java.util.Collection;

public interface ReactiveFortuneRepository {

    Publisher<Void> initDb(Collection<Fortune> fortunes);

    Publisher<Fortune> findAll();

}
