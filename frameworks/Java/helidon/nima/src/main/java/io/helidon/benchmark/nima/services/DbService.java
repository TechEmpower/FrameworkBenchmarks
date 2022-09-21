
package io.helidon.benchmark.nima.services;

import java.util.Collections;
import java.util.List;

import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.benchmark.nima.models.World;
import io.helidon.common.parameters.Parameters;
import io.helidon.nima.webserver.http.HttpRules;
import io.helidon.nima.webserver.http.HttpService;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;
import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonBuilderFactory;
import jakarta.json.JsonObject;

import static io.helidon.benchmark.nima.Main.SERVER;

public class DbService implements HttpService {
    private static final JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    private final DbRepository repository;

    public DbService(DbRepository repository) {
        this.repository = repository;
    }

    @Override
    public void routing(HttpRules httpRules) {
        httpRules.get("/db", this::db);
        httpRules.get("/queries", this::queries);
        httpRules.get("/updates", this::updates);
    }

    private void db(ServerRequest req, ServerResponse res) {
        res.header(SERVER);
        World world = repository.getWorld();
        res.send(world.toJson());
    }

    private void queries(ServerRequest req, ServerResponse res) {
        res.header(SERVER);
        int count = parseQueryCount(req.query());
        res.send(repository.getWorldsAsJson(count));
    }

    private void updates(ServerRequest req, ServerResponse res) {
        res.header(SERVER);
        int count = parseQueryCount(req.query());
        List<World> worlds = repository.updateWorlds(count);
        JsonArrayBuilder arrayBuilder = JSON.createArrayBuilder();
        for (World world : worlds) {
            JsonObject json = world.toJson();
            arrayBuilder.add(json);
        }
        res.send(arrayBuilder.build());
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