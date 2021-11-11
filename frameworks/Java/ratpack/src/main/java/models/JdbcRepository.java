package models;

import ratpack.exec.Blocking;
import ratpack.exec.Promise;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class JdbcRepository implements DbRepository {
    private final DataSource dataSource;

    public JdbcRepository(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public Promise<World> getWorld(int id) {
        return getWorlds(new int[]{id}).map(worlds -> worlds.get(0));
    }

    @Override
    public Promise<List<World>> getWorlds(int[] ids) {
        return Blocking.get(() -> {
            World[] worlds = new World[ids.length];
            try (Connection connection = dataSource.getConnection()) {
                Arrays.setAll(worlds, value -> {
                    try {
                        PreparedStatement statement = connection.prepareStatement("SELECT id, randomnumber FROM world WHERE id = ?");
                        statement.setInt(1, ids[value]);
                        ResultSet rs = statement.executeQuery();
                        rs.next();
                        World world = new World(rs.getInt(1), rs.getInt(2));
                        statement.close();
                        return world;
                    } catch (SQLException e) {
                        throw new RuntimeException(e);
                    }
                });
            }

            return Arrays.asList(worlds);
        });
    }

    @Override
    public Promise<List<World>> findAndUpdateWorlds(int[] ids, int[] randomNumbers) {
        return getWorlds(ids).flatMap(worlds -> {
            return Blocking.get(() -> {
                try (Connection connection = dataSource.getConnection()) {
                    int i = 0;
                    for(World world: worlds) {
                        PreparedStatement statement = connection.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?");
                        world.randomNumber = randomNumbers[i++];
                        statement.setInt(1, world.randomNumber);
                        statement.setInt(2, world.id);
                        statement.executeUpdate();
                        statement.close();
                    }
                    return worlds;
                }
            });
        });
    }

    @Override
    public Promise<List<Fortune>> fortunes() {
        return Blocking.get(() -> {
            try (Connection connection = dataSource.getConnection()) {
                PreparedStatement statement = connection.prepareStatement("SELECT id, message FROM fortune");
                ResultSet rs = statement.executeQuery();

                List<Fortune> fortunes = new ArrayList<>();
                while (rs.next()) {
                    fortunes.add(new Fortune(rs.getInt(1), rs.getString(2)));
                }
                statement.close();
                return fortunes;
            }
        });
    }
}
