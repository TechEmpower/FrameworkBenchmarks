package handlers;

import models.World;
import ratpack.exec.Blocking;
import ratpack.handling.Context;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;

import static ratpack.jackson.Jackson.json;

public class UpdateHandler extends BaseWorldHandler {
    public void handle(Context ctx, DataSource datasource) throws Exception {
        int queries = parseQueryCount(ctx);

        Blocking.get(() -> {
            try (Connection connection = datasource.getConnection()) {
                World[] worlds = new World[queries];
                Arrays.setAll(worlds, i -> getWorld(connection));
                update(worlds, connection);
                return worlds;
            }
        }).then(result -> ctx.render(json(result)));
    }

    private void update(World[] worlds, Connection connection) {
        try {
            for (World world : worlds) {
                PreparedStatement statement = connection.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?");
                world.randomNumber = randomWorldNumber();
                statement.setInt(1, world.randomNumber);
                statement.setInt(2, world.id);
                statement.executeUpdate();
                statement.close();
            }
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }
}
