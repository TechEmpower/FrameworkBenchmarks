package io.helidon.benchmark.services;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;

import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.World;
import io.helidon.common.http.Parameters;
import io.helidon.common.reactive.Multi;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public class DbService implements Service {

    private final DbRepository repository;

    public DbService(DbRepository repository) {
        this.repository = repository;
    }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/db", this::db);
        rules.get("/queries", this::queries);
        rules.get("/updates", this::updates);
    }

    private void db(final ServerRequest request,
                    final ServerResponse response) {
        repository.getWorld(randomWorldNumber())
                .forSingle(world -> response.send(world.toJson()));
    }

    private void queries(final ServerRequest request,
                         final ServerResponse response) {

        Multi.range(0, parseQueryCount(request.queryParams()))
                .flatMap(i -> repository.getWorld(randomWorldNumber()))
                .map(World::toJson)
                .reduce(JSON::createArrayBuilder, JsonArrayBuilder::add)
                .map(JsonArrayBuilder::build)
                .onError(response::send)
                .forSingle(response::send);

    }

    private void updates(final ServerRequest request,
                         final ServerResponse response) {

        Multi.range(0, parseQueryCount(request.queryParams()))
                .flatMap(i -> repository.getWorld(randomWorldNumber()), 1, false, 1)
                .flatMap(world -> {
                    world.randomNumber = randomWorldNumber();
                    return repository.updateWorld(world);
                }, 1, false, 1)
                .map(World::toJson)
                .reduce(JSON::createArrayBuilder, JsonArrayBuilder::add)
                .map(JsonArrayBuilder::build)
                .onError(response::send)
                .forSingle(response::send);
    }

    private static final JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(Parameters parameters) {
        List<String> values = parameters.all("queries");

        if (values.isEmpty()) {
            return 1;
        }

        String first = values.get(0);

        int parsedValue;
        try {
            parsedValue = Integer.parseInt(first, 10);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}