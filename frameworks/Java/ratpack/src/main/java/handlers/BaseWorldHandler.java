package handlers;

import models.World;
import ratpack.handling.Context;
import ratpack.handling.InjectionHandler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

public abstract class BaseWorldHandler extends InjectionHandler {
    protected int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    protected int parseQueryCount(Context ctx) {
        String textValue = ctx.getRequest().getQueryParams().get("queries");

        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }

    protected World getWorld(Connection connection) {
        try {
            PreparedStatement statement = connection.prepareStatement("SELECT id, randomnumber FROM world WHERE id = ?");
            statement.setInt(1, randomWorldNumber());
            ResultSet rs = statement.executeQuery();
            rs.next();
            World world = new World(rs.getInt(1), rs.getInt(2));
            statement.close();
            return world;
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }
}
