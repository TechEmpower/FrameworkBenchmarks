package io.helidon.benchmark.services;

import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.World;
import io.helidon.common.http.DataChunk;
import io.helidon.common.http.MediaType;
import io.helidon.common.http.Parameters;
import io.helidon.common.reactive.ReactiveStreamsAdapter;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;
import io.reactivex.Flowable;
import io.reactivex.Single;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

public class DbService implements Service {

    private final DbRepository repository;
    private JsonWriterFactory jsonWriterFactory;

    public DbService(DbRepository repository) {
        this.repository = repository;

        this.jsonWriterFactory = Json.createWriterFactory(null);
    }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/db", this::db);
        rules.get("/queries", this::queries);
        rules.get("/updates", this::updates);
    }

    private void db(final ServerRequest request,
                          final ServerResponse response) {
        Single<DataChunk> result = repository.getWorld(randomWorldNumber())
                .map(World::toJson)
                .map(this::getChunk);

        send(response, result);
    }

    private void queries(final ServerRequest request,
                           final ServerResponse response) {
        Flowable<JsonObject>[] worlds = new Flowable[parseQueryCount(request.queryParams())];
        Arrays.setAll(worlds, i -> repository.getWorld(randomWorldNumber()).map(World::toJson).toFlowable());

        Single<DataChunk> result = marshall(worlds);

        send(response, result);
    }

    private void updates(final ServerRequest request,
                         final ServerResponse response) {
        Flowable<JsonObject>[] worlds = new Flowable[parseQueryCount(request.queryParams())];

        Arrays.setAll(worlds, i -> {
            return repository.getWorld(randomWorldNumber()).flatMapPublisher(world -> {
                world.randomNumber = randomWorldNumber();
                return repository.updateWorld(world).map(World::toJson).toFlowable();
            });
        });

        Single<DataChunk> result = marshall(worlds);

        send(response, result);
    }

    private Single<DataChunk> marshall(Flowable<JsonObject>[] worlds) {
        return Flowable.mergeArray(worlds)
                .toList()
                .map(this::buildArray)
                .map(this::getChunk)
                .doOnError(Throwable::printStackTrace);
    }

    private JsonArray buildArray(List<JsonObject> jsonObjects) {
        return jsonObjects.stream().reduce(
                Json.createArrayBuilder(),
                JsonArrayBuilder::add,
                JsonArrayBuilder::addAll)
                .build();
    }

    private void send(final ServerResponse response, Single<DataChunk> result) {
        response.headers().contentType(MediaType.APPLICATION_JSON);
        response.send(ReactiveStreamsAdapter.publisherToFlow(result.toFlowable()));
    }

    private DataChunk getChunk(JsonStructure jsonStructure) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        JsonWriter writer = jsonWriterFactory.createWriter(baos);
        writer.write(jsonStructure);
        writer.close();

        return DataChunk.create(baos.toByteArray());
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(Parameters parameters) {
        Optional<String> textValue = parameters.first("queries");

        if (!textValue.isPresent()) {
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
