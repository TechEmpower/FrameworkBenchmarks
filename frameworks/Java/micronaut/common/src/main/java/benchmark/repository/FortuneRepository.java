package benchmark.repository;

import benchmark.model.Fortune;
import org.reactivestreams.Publisher;

import java.util.Collection;

public interface FortuneRepository {

    void initDb(Collection<Fortune> fortunes);

    Collection<Fortune> findAll();

}
