package io.helidon.benchmark.services;

import io.helidon.benchmark.models.Repository;
import io.helidon.benchmark.models.World;
import io.helidon.common.media.type.MediaTypes;
import io.helidon.common.parameters.Parameters;
import io.helidon.nima.webserver.http.HttpRules;
import io.helidon.nima.webserver.http.HttpService;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;

import javax.json.Json;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

public class DbService implements HttpService {

    private final Repository repository;

    public DbService(Repository repository) {
        this.repository = repository;
    }

    private void db(final ServerRequest request, final ServerResponse response) {
        response.headers().contentType(MediaTypes.APPLICATION_JSON);
        response.send(repository.getWorld(randomWorldNumber()).toJson().toString());
    }

    private void queries(final ServerRequest request, final ServerResponse response) {
        var count = parseQueryCount(request.query());
        var arrayBuilder = Json.createArrayBuilder();

        randomWorldNumbers()
                .limit(count)
                .mapToObj(repository::getWorld)
                .map(World::toJson)
                .forEach(arrayBuilder::add);

        response.headers().contentType(MediaTypes.APPLICATION_JSON);
        response.send(arrayBuilder.build().toString());
    }

    private void updates(final ServerRequest request, final ServerResponse response) {
        var count = parseQueryCount(request.query());
        var arrayBuilder = Json.createArrayBuilder();

        var worlds = randomWorldNumbers()
            .mapToObj(repository::getWorld)
            .limit(count)
            .toList();

        repository.updateWorlds(worlds);

        for (var world : worlds)
            arrayBuilder.add(world.toJson());

        response.headers().contentType(MediaTypes.APPLICATION_JSON);
        response.send(arrayBuilder.build().toString());
    }

    private static final int MIN_WORLD_NUMBER = 1;
    private static final int MAX_WORLD_NUMBER_PLUS_ONE = 10_001;

    private static int randomWorldNumber() {
        return ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE);
    }

    private static IntStream randomWorldNumbers() {
        return ThreadLocalRandom.current().ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE);
    }

    private int parseQueryCount(Parameters parameters) {
        var values = parameters.all("queries");

        if (values.isEmpty())
            return 1;

        try {
            var parsedValue = Integer.parseInt(values.get(0), 10);
            return Math.min(500, Math.max(1, parsedValue));
        } catch (NumberFormatException e) {
            return 1;
        }
    }

    @Override
    public void routing(HttpRules httpRules) {
        httpRules.get("/db", this::db);
        httpRules.get("/queries", this::queries);
        httpRules.get("/updates", this::updates);
    }
}