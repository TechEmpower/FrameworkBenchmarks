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
import javax.json.JsonWriterFactory;

import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.World;
import io.helidon.common.http.Parameters;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;
import io.reactivex.Flowable;
import io.reactivex.Single;

public class DbService implements Service {

    private final DbRepository repository;
    private JsonWriterFactory jsonWriterFactory;
    private JsonBuilderFactory jsonBuilderFactory;

    public DbService(DbRepository repository) {
        this.repository = repository;

        this.jsonWriterFactory = Json.createWriterFactory(null);
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
                .subscribe(jsonObject -> response.send(jsonObject));
    }

    private void queries(final ServerRequest request,
                           final ServerResponse response) {
        Flowable<JsonObject>[] worlds = new Flowable[parseQueryCount(request.queryParams())];
        Arrays.setAll(worlds, i -> repository.getWorld(randomWorldNumber()).map(World::toJson).toFlowable());

        marshall(worlds).subscribe(jsonObject -> response.send(jsonObject));
    }

    private void updates(final ServerRequest request,
                         final ServerResponse response) {
        Flowable<JsonObject>[] worlds = new Flowable[parseQueryCount(request.queryParams())];

        Arrays.setAll(worlds, i -> repository.getWorld(randomWorldNumber()).flatMapPublisher(world -> {
            world.randomNumber = randomWorldNumber();
            return repository.updateWorld(world).map(World::toJson).toFlowable();
        }));

        marshall(worlds).subscribe(jsonObject -> response.send(jsonObject));
    }

    private Single<JsonArray> marshall(Flowable<JsonObject>[] worlds) {
        return Flowable.mergeArray(worlds)
                .toList()
                .map(this::buildArray)
                .doOnError(Throwable::printStackTrace);
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
