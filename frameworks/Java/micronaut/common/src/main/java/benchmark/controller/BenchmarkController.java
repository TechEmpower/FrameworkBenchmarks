package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.FortuneRepository;
import benchmark.repository.WorldRepository;
import io.micronaut.context.annotation.Requires;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.micronaut.scheduling.TaskExecutors;
import io.micronaut.scheduling.annotation.ExecuteOn;
import views.fortunes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Comparator.comparing;

@ExecuteOn(TaskExecutors.IO)
@Requires(missingBeans = ReactiveBenchmarkController.class)
@Controller
public class BenchmarkController extends AbstractBenchmarkController {

    private final WorldRepository worldRepository;
    private final FortuneRepository fortuneRepository;

    public BenchmarkController(WorldRepository worldRepository,
                               FortuneRepository fortuneRepository) {
        this.worldRepository = worldRepository;
        this.fortuneRepository = fortuneRepository;
    }

    @Get("/prepare-data-for-test")
    public void prepareDataForTest() {
        worldRepository.initDb(createWords());
        fortuneRepository.initDb(createFortunes());
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
    @Get("/db")
    public World db() {
        return worldRepository.findById(randomId());
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
    @Get("/queries")
    public List<World> queries(@QueryValue String queries) {
        int count = parseQueryCount(queries);
        List<Integer> ids = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            ids.add(randomId());
        }
        return worldRepository.findByIds(ids);
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#fortunes
    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    public HttpResponse<String> fortune() {
        Collection<Fortune> all = fortuneRepository.findAll();
        List<Fortune> fortunesList = new ArrayList<>(all.size() + 1);
        fortunesList.add(new Fortune(0, "Additional fortune added at request time."));
        fortunesList.addAll(all);
        fortunesList.sort(comparing(Fortune::getMessage));
        String body = fortunes.template(fortunesList).render().toString();
        return HttpResponse.ok(body).contentType("text/html;charset=utf-8");
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
    @Get("/updates")
    public List<World> updates(@QueryValue String queries) {
        List<World> worldList = queries(queries);
        for (World world : worldList) {
            world.setRandomNumber(randomWorldNumber());
        }
        worldList.sort(Comparator.comparingInt(World::getId)); // Avoid deadlock
        worldRepository.updateAll(worldList);
        return worldList;
    }
    
}
