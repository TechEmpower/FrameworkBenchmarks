package benchmark;

import benchmark.model.Fortune;
import benchmark.model.World;
import org.springframework.data.mongodb.core.FindAndModifyOptions;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.reactive.result.view.Rendering;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.Arrays;
import java.util.List;

import static java.util.Comparator.comparing;
import static org.springframework.data.mongodb.core.FindAndModifyOptions.*;
import static org.springframework.data.mongodb.core.query.Criteria.*;
import static org.springframework.data.mongodb.core.query.Query.*;
import static org.springframework.data.mongodb.core.query.Update.*;

@Controller()
@RequestMapping("/mongo")
public final class NoSQLController extends BaseDBController {

    private final ReactiveMongoTemplate mongoTemplate;

    public NoSQLController(ReactiveMongoTemplate mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
    }

    @GetMapping(value = "/db", produces = "application/json")
    @ResponseBody
    public Mono<World> db() {
        return mongoTemplate.findById(randomWorldNumber(), World.class);
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

        Arrays.setAll(worlds, i -> mongoTemplate.findAndModify(
                query(where("id").is(randomWorldNumber())),
                update("randomNumber", randomWorldNumber()),
                options().returnNew(true),
                World.class));

        return Flux.merge(worlds).collectList();
    }

    @GetMapping(value = "/fortunes")
    public Rendering fortunes() {
        Mono<List<Fortune>> result = mongoTemplate.findAll(Fortune.class).collectList().flatMap(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(comparing(fortune -> fortune.message));
            return Mono.just(fortunes);
        });

        return Rendering.view("fortunes").modelAttribute("fortunes", result).build();
    }
}
