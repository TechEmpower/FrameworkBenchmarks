package benchmark.web;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.DbRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import static java.util.Comparator.comparing;

@Component
public class DbHandler {

    private final DbRepository dbRepository;

    public DbHandler(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    public Mono<ServerResponse> db(ServerRequest request) {
        int id = randomWorldNumber();
        Mono<World> world = dbRepository.getWorld(id)
                .switchIfEmpty(Mono.error(new Exception("No World found with Id: " + id)));

        return ServerResponse.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(world, World.class);
    }

    public Mono<ServerResponse> queries(ServerRequest request) {
        int queries = parseQueryCount(request.queryParams().getFirst("queries"));

        Mono<List<World>> worlds = Flux.range(0, queries)
                .flatMap(i -> dbRepository.getWorld(randomWorldNumber()))
                .collectList();

        return ServerResponse.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(worlds, new ParameterizedTypeReference<List<World>>() {
                });
    }

    private static int parseQueryCount(String maybeTextValue) {
        if (maybeTextValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(maybeTextValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }

    public Mono<ServerResponse> updates(ServerRequest request) {
        int queries = parseQueryCount(request.queryParams().getFirst("queries"));

        Mono<List<World>> worlds = Flux.range(0, queries)
                .flatMap(i -> dbRepository.findAndUpdateWorld(randomWorldNumber(), randomWorldNumber()))
                .collectList();

        return ServerResponse.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(worlds, new ParameterizedTypeReference<List<World>>() {
                });
    }

    public Mono<ServerResponse> fortunes(ServerRequest request) {
        Mono<List<Fortune>> result = dbRepository.fortunes().collectList().flatMap(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(comparing(fortune -> fortune.message));
            return Mono.just(fortunes);
        });

        return ServerResponse.ok()
                .render("fortunes", Collections.singletonMap("fortunes", result));
    }

    private static int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}