package benchmark.web;

import java.util.List;

import benchmark.Utils;
import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.DbRepository;
import io.jstach.jstachio.JStachio;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

@Component
public class DbHandler {

    private static final String CONTENT_TYPE_VALUE = "text/html; charset=utf-8";

    private final DbRepository dbRepository;

    public DbHandler(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    public Mono<ServerResponse> db(ServerRequest request) {
        int id = Utils.randomWorldNumber();
        Mono<World> world = dbRepository.getWorld(id)
                .switchIfEmpty(Mono.error(new Exception("No World found with Id: " + id)));

        return ServerResponse.ok()
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(world, World.class);
    }

    public Mono<ServerResponse> queries(ServerRequest request) {
        int queries = parseQueryCount(request.queryParams().getFirst("queries"));

        Mono<List<World>> worlds = Flux.fromStream(Utils.randomWorldNumbers().limit(queries).boxed())
                .flatMap(dbRepository::getWorld)
                .collectList();

        return ServerResponse.ok()
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
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

        Mono<List<World>> worlds = Flux.fromStream(Utils.randomWorldNumbers().limit(queries).boxed())
                .flatMap(i -> dbRepository.findAndUpdateWorld(i, Utils.randomWorldNumber()))
                .collectList();

        return ServerResponse.ok()
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(worlds, new ParameterizedTypeReference<List<World>>() {
                });
    }

    public Mono<ServerResponse> fortunes(ServerRequest request) {
        return dbRepository.fortunes()
                .concatWith(Mono.just(new Fortune(0, "Additional fortune added at request time.")))
                .collectSortedList()
                .flatMap(fortunes ->
                        ServerResponse.ok()
                                .header(HttpHeaders.CONTENT_TYPE, CONTENT_TYPE_VALUE)
                                .bodyValue(JStachio.render(new Fortunes(fortunes))));
    }

}