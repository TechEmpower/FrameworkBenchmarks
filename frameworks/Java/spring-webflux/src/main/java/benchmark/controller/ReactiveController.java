package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.DbRepository;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.reactive.result.view.Rendering;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Controller()
@Profile(value = {"jdbc", "pgclient", "mongo"})
public final class ReactiveController {

    private final DbRepository dbRepository;

    public ReactiveController(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    @GetMapping(value = "/db", produces = "application/json")
    @ResponseBody
    public Mono<World> db() {
        return dbRepository.getWorld(randomWorldNumber());
    }

    @GetMapping(value = "/queries", produces = "application/json")
    @ResponseBody
    public Mono<List<World>> queries(@RequestParam String queries) {
        Mono<World>[] worlds = new Mono[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> db());

        return Flux.merge(worlds).collectList();
    }

    @GetMapping(value = "/updates", produces = "application/json")
    @ResponseBody
    public Mono<List<World>> updates(@RequestParam String queries) {
        Mono<World>[] worlds = new Mono[parseQueryCount(queries)];

        Arrays.setAll(worlds, i -> dbRepository.findAndUpdateWorld(randomWorldNumber(), randomWorldNumber()));

        return Flux.merge(worlds).collectList();
    }

    @GetMapping(value = "/fortunes")
    public Rendering fortunes() {
        Mono<List<Fortune>> result = dbRepository.fortunes().collectList().flatMap(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(comparing(fortune -> fortune.message));
            return Mono.just(fortunes);
        });

        return Rendering.view("fortunes").modelAttribute("fortunes", result).build();
    }

    protected int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    protected int parseQueryCount(String textValue) {
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
