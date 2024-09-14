package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.FortuneRepository;
import benchmark.repository.WorldRepository;
import io.micronaut.context.annotation.Requires;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Requires(beans = {WorldRepository.class, FortuneRepository.class})
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
    public List<Fortune> fortune() {
        return prepareFortunes(fortuneRepository.findAll());
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
