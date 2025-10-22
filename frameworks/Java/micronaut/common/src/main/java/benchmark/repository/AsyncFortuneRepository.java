package benchmark.repository;

import benchmark.model.Fortune;
import org.reactivestreams.Publisher;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

public interface AsyncFortuneRepository {

    CompletionStage<?> initDb(Collection<Fortune> fortunes);

    CompletionStage<List<Fortune>> findAll();

}
