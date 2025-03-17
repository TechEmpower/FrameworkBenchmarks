package io.helidon.benchmark.nima.services;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.common.mapper.OptionalValue;
import io.helidon.common.parameters.Parameters;
import io.helidon.http.HeaderValues;
import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.HttpService;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

import static io.helidon.benchmark.nima.JsonSerializer.serialize;
import static io.helidon.benchmark.nima.Main.SERVER;
import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;

public class DbService implements HttpService {

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
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            res.header(SERVER);
            res.header(HeaderValues.CONTENT_TYPE_JSON);
            serialize(repository.getWorld(randomWorldNumber()), stream);
            res.send(stream.buffer().data(), 0, stream.buffer().tail());
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

    private void queries(ServerRequest req, ServerResponse res) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            res.header(SERVER);
            res.header(HeaderValues.CONTENT_TYPE_JSON);
            int count = parseQueryCount(req.query());
            serialize(repository.getWorlds(count), stream);
            res.send(stream.buffer().data(), 0, stream.buffer().tail());
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

    private void updates(ServerRequest req, ServerResponse res) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            res.header(SERVER);
            res.header(HeaderValues.CONTENT_TYPE_JSON);
            int count = parseQueryCount(req.query());
            serialize(repository.updateWorlds(count), stream);
            res.send(stream.buffer().data(), 0, stream.buffer().tail());
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

    private int parseQueryCount(Parameters parameters) {
        OptionalValue<String> value = parameters.first("queries");
        if (value.isEmpty()) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(value.get(), 10);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
