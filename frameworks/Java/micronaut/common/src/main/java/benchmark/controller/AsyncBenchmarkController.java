package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.AsyncFortuneRepository;
import benchmark.repository.AsyncWorldRepository;
import io.micronaut.context.annotation.Requires;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CompletionStage;

import static java.util.Comparator.comparing;

@Requires(beans = {AsyncWorldRepository.class, AsyncFortuneRepository.class})
@Controller
public class AsyncBenchmarkController extends AbstractBenchmarkController {

    private final AsyncWorldRepository worldRepository;
    private final AsyncFortuneRepository fortuneRepository;

    public AsyncBenchmarkController(AsyncWorldRepository worldRepository,
                                    AsyncFortuneRepository fortuneRepository) {
        this.worldRepository = worldRepository;
        this.fortuneRepository = fortuneRepository;
    }

    @Get("/prepare-data-for-test")
    public CompletionStage<?> prepareDataForTest() {
        return worldRepository.initDb(createWords()).thenCompose(ignore -> fortuneRepository.initDb(createFortunes()));
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
    @Get("/db")
    public CompletionStage<World> db() {
        return worldRepository.findById(randomId());
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
    @Get("/queries")
    public CompletionStage<List<World>> queries(@QueryValue String queries) {
        int count = parseQueryCount(queries);
        List<Integer> ids = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            ids.add(randomId());
        }
        return worldRepository.findByIds(ids);
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#fortunes
    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    public CompletionStage<List<Fortune>> fortune() {
        return fortuneRepository.findAll().thenApply(this::prepareFortunes);
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
    @Get("/updates")
    public CompletionStage<List<World>> updates(@QueryValue String queries) {
        return queries(queries).thenCompose(worlds -> {
            for (World world : worlds) {
                world.setRandomNumber(randomWorldNumber());
            }
            worlds.sort(Comparator.comparingInt(World::getId)); // Avoid deadlock
            return worldRepository.updateAll(worlds).thenApply(ignore -> worlds);
        });
    }

}
