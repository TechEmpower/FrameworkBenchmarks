package io.helidon.benchmark.services;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;

import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.World;
import io.helidon.common.http.Parameters;
import io.helidon.common.reactive.Multi;
import io.helidon.common.reactive.Single;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public class DbService implements Service {

    private final DbRepository repository;
    private JsonBuilderFactory jsonBuilderFactory;

    public DbService(DbRepository repository) {
        this.repository = repository;

        this.jsonBuilderFactory = Json.createBuilderFactory(Collections.emptyMap());
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
                .map(World::toJson)
                .thenAccept(response::send)
                .exceptionally(response::send);
    }

    private void queries(final ServerRequest request,
                           final ServerResponse response) {
        @SuppressWarnings("unchecked")
        Single<JsonObject>[] worlds = new Single[parseQueryCount(request.queryParams())];
        Arrays.setAll(worlds, i -> repository.getWorld(randomWorldNumber()).map(World::toJson));

        marshall(worlds)
                .thenAccept(response::send)
                .exceptionally(response::send);
    }

    private void updates(final ServerRequest request,
                         final ServerResponse response) {
        @SuppressWarnings("unchecked")
        Single<JsonObject>[] worlds = new Single[parseQueryCount(request.queryParams())];

        Arrays.setAll(worlds, i -> {
            return repository.getWorld(randomWorldNumber()).flatMapSingle(world -> {
                world.randomNumber = randomWorldNumber();
                return repository.updateWorld(world).map(World::toJson);
            });
        });

        marshall(worlds)
                .thenAccept(response::send)
                .exceptionally(response::send);
    }

    private Single<JsonArray> marshall(Single<JsonObject>[] worlds) {
        Multi.concatArray(worlds).onComplete(() -> System.out.println("Whoo hoo!"));
        return Multi.concatArray(worlds)
                .collectList()
                .map(this::buildArray)
                .onError(Throwable::printStackTrace);
    }

    private JsonArray buildArray(List<JsonObject> jsonObjects) {
        return jsonObjects.stream().reduce(
                jsonBuilderFactory.createArrayBuilder(),
                JsonArrayBuilder::add,
                JsonArrayBuilder::addAll)
                .build();
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(Parameters parameters) {
        Optional<String> textValue = parameters.first("queries");

        if (textValue.isEmpty()) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue.get());
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
