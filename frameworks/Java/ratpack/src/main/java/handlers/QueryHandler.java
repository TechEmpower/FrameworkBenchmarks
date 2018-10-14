package handlers;

import models.DbRepository;
import models.JdbcRepository;
import models.World;
import ratpack.exec.Blocking;
import ratpack.handling.Context;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Arrays;

import static ratpack.jackson.Jackson.json;

public class QueryHandler extends BaseWorldHandler {
    public void handle(Context ctx, DbRepository repository) {
        int queries = parseQueryCount(ctx);

        repository.getWorlds(getNumbers(queries)).then(result -> ctx.render(json(result)));
    }
}
