package benchmark;

import benchmark.model.Fortune;
import benchmark.model.World;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.reactive.result.view.Rendering;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Controller
public final class SQLController extends BaseDBController {

    private final JdbcTemplate jdbcTemplate;
    private final Scheduler scheduler;

    public SQLController(JdbcTemplate jdbcTemplate, Scheduler scheduler) {
        this.jdbcTemplate = jdbcTemplate;
        this.scheduler = scheduler;
    }

    @GetMapping(value = "/db", produces = "application/json")
    @ResponseBody
    public Mono<World> db() {
        return Mono.fromCallable(() -> randomWorld()).subscribeOn(scheduler);
    }

    @GetMapping(value = "/queries", produces = "application/json")
    @ResponseBody
    public Mono<World[]> queries(@RequestParam String queries) {
        return Mono.fromCallable(() -> {
            var worlds = new World[parseQueryCount(queries)];
            Arrays.setAll(worlds, i -> randomWorld());
            return worlds;
        }).subscribeOn(scheduler);
    }

    @GetMapping(value = "/updates", produces = "application/json")
    @ResponseBody
    public Mono<World[]> updates(@RequestParam String queries) {
        return Mono.fromCallable(() -> {
            var worlds = new World[parseQueryCount(queries)];
            Arrays.setAll(worlds, i -> randomWorld());
            for (var world : worlds) {
                world.randomNumber = randomWorldNumber();
                jdbcTemplate.update(
                        "UPDATE world SET randomnumber = ? WHERE id = ?",
                        world.randomNumber,
                        world.id);
            }
            return worlds;
        }).subscribeOn(scheduler);
    }

    @GetMapping(value = "/fortunes")
    public Rendering fortunes() {
        Mono<List<Fortune>> fortunes = Mono.fromCallable(() -> {
            var list =
                    jdbcTemplate.query(
                            "SELECT * FROM fortune",
                            (rs, rn) -> new Fortune(rs.getInt("id"), rs.getString("message")));

            list.add(new Fortune(0, "Additional fortune added at request time."));
            list.sort(comparing(fortune -> fortune.message));
            return list;
        }).subscribeOn(scheduler);

        return Rendering.view("fortunes").modelAttribute("fortunes", fortunes).build();
    }

    private World randomWorld() {
        return jdbcTemplate.queryForObject(
                "SELECT * FROM world WHERE id = ?",
                (rs, rn) -> new World(rs.getInt("id"), rs.getInt("randomnumber")),
                randomWorldNumber());
    }
}
