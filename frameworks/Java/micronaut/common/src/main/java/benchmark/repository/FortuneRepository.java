package benchmark.repository;

import benchmark.model.Fortune;
import org.reactivestreams.Publisher;

import java.util.Collection;
import java.util.List;

public interface FortuneRepository {

    void initDb(Collection<Fortune> fortunes);

    List<Fortune> findAll();

}
