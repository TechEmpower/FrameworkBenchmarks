package handlers;

import models.DbRepository;
import models.JdbcRepository;
import ratpack.handling.Context;

import static ratpack.jackson.Jackson.json;

public class UpdateHandler extends BaseWorldHandler {
    public void handle(Context ctx, DbRepository repository) {
        int queries = parseQueryCount(ctx);

        repository.findAndUpdateWorlds(getNumbers(queries), getNumbers(queries)).then(result -> ctx.render(json(result)));
    }
}
