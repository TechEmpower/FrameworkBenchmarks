package handlers;

import models.DbRepository;
import models.JdbcRepository;
import ratpack.exec.Blocking;
import ratpack.handling.Context;

import javax.sql.DataSource;
import java.sql.Connection;

import static ratpack.jackson.Jackson.json;

public class DbHandler extends BaseWorldHandler {
    public void handle(Context ctx, DbRepository repository) {
        repository.getWorld(randomWorldNumber()).then(result -> ctx.render(json(result)));
    }
}
