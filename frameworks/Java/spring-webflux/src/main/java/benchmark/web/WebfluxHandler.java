package benchmark.web;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.DbRepository;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Component
public class WebfluxHandler {
    private final DbRepository dbRepository;

    public WebfluxHandler(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    public Mono<ServerResponse> plaintext(ServerRequest request) {
        return ServerResponse.ok()
                .contentType(MediaType.TEXT_PLAIN)
                .body(Mono.just("Hello, World!"), String.class);
    }

    public Mono<ServerResponse> json(ServerRequest request) {
        return ServerResponse.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(Mono.just(Map.of("message", "Hello, World!")), Map.class);
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
        int queries = getQueries(request);

        Mono<List<World>> worlds = Flux.range(0, queries)
                .flatMap(i -> dbRepository.getWorld(randomWorldNumber()))
                .collectList();

        return ServerResponse.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(worlds, new ParameterizedTypeReference<List<World>>() {
                });
    }

    private static int parseQueryCount(Optional<String> maybeTextValue) {
        if (!maybeTextValue.isPresent()) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(maybeTextValue.get());
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }

    public Mono<ServerResponse> updates(ServerRequest request) {
        int queries = getQueries(request);
        
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

    private static int getQueries(ServerRequest request) {
        return parseQueryCount(request.queryParam("queries"));
    }

    private static int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
