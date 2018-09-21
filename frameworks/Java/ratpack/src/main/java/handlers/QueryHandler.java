package handlers;

import models.World;
import ratpack.exec.Blocking;
import ratpack.handling.Context;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Arrays;

import static ratpack.jackson.Jackson.json;

public class QueryHandler extends BaseWorldHandler {
    public void handle(Context ctx, DataSource datasource) {
        int queries = parseQueryCount(ctx);

        Blocking.get(() -> {
            try (Connection connection = datasource.getConnection()) {
                World[] worlds = new World[queries];
                Arrays.setAll(worlds, i -> getWorld(connection));
                return worlds;
            }
        }).then(result -> ctx.render(json(result)));
    }
}
