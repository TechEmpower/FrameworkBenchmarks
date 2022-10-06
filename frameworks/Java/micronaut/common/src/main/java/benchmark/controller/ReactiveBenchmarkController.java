package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.ReactiveFortuneRepository;
import benchmark.repository.ReactiveWorldRepository;
import io.micronaut.context.annotation.Requires;
import io.micronaut.core.async.annotation.SingleResult;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MutableHttpResponse;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import views.fortunes;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Comparator.comparing;

@Requires(beans = {ReactiveFortuneRepository.class, ReactiveWorldRepository.class})
@Controller
public class ReactiveBenchmarkController {

    private final ReactiveWorldRepository worldRepository;
    private final ReactiveFortuneRepository fortuneRepository;

    public ReactiveBenchmarkController(ReactiveWorldRepository worldRepository,
                                       ReactiveFortuneRepository fortuneRepository) {
        this.worldRepository = worldRepository;
        this.fortuneRepository = fortuneRepository;
    }

    @Get("/prepare-data-for-test")
    public Mono<Void> prepareDataForTest() {
        return createWorlds().then(createFortunes());
    }

    private Mono<Void> createWorlds() {
        List<Integer> ids = IntStream.range(1, 10001).boxed().collect(Collectors.toList());
        Collections.shuffle(ids);
        List<World> worlds = new ArrayList<>(ids.size());
        for (Integer id : ids) {
            worlds.add(new World(id, randomWorldNumber()));
        }
        return Mono.from(worldRepository.initDb(worlds));
    }

    private Mono<Void> createFortunes() {
        List<Integer> fortuneMessages = IntStream.range(0, 10).boxed().collect(Collectors.toList());
        List<Fortune> fortunes = new ArrayList<>(fortuneMessages.size());
        for (Integer number : fortuneMessages) {
            fortunes.add(new Fortune(number + 1, "message" + number));
        }
        Collections.shuffle(fortunes);
        return Mono.from(fortuneRepository.initDb(fortunes));
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
    @Get("/db")
    @SingleResult
    public Publisher<World> db() {
        return worldRepository.findById(randomId());
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
    @Get("/queries")
    @SingleResult
    public Publisher<List<World>> queries(@QueryValue String queries) {
        int count = parseQueryCount(queries);
        List<Integer> ids = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            ids.add(randomId());
        }
        return worldRepository.findByIds(ids);
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#fortunes
    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    @SingleResult
    public Mono<HttpResponse<String>> fortune() {
        return Flux.from(fortuneRepository.findAll()).collectList().map(fortuneList -> {
            fortuneList.add(new Fortune(0, "Additional fortune added at request time."));
            fortuneList.sort(comparing(Fortune::getMessage));
            String body = fortunes.template(fortuneList).render().toString();
            return HttpResponse.ok(body).contentType("text/html;charset=utf-8");
        });
    }

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
    @Get("/updates")
    @SingleResult
    public Publisher<List<World>> updates(@QueryValue String queries) {
        return Flux.from(queries(queries)).flatMap(worlds -> {
            for (World world : worlds) {
                world.setRandomNumber(randomWorldNumber());
            }
            return Mono.from(worldRepository.updateAll(worlds)).thenReturn(worlds);
        });
    }

    private int randomId() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
