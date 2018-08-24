package handlers;

import ratpack.exec.Blocking;
import ratpack.handling.Context;

import javax.sql.DataSource;
import java.sql.Connection;

import static ratpack.jackson.Jackson.json;

public class DbHandler extends BaseWorldHandler {
    public void handle(Context ctx, DataSource datasource) {
        Blocking.get(() -> {
            try (Connection connection = datasource.getConnection()) {
                return getWorld(connection);
            }
        }).then(result -> ctx.render(json(result)));
    }
}
