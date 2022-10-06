package benchmark;

import benchmark.model.World;
import benchmark.repository.WorldRepository;
import jakarta.inject.Singleton;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Singleton
public class JdbcWorldRepository implements WorldRepository {

    private final DataSource dataSource;

    public JdbcWorldRepository(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void initDb(Collection<World> worlds) {
        try (Connection connection = dataSource.getConnection()) {
            connection.createStatement().execute("DROP TABLE IF EXISTS World;");
            connection.createStatement().execute("CREATE TABLE World (id INTEGER NOT NULL,randomNumber INTEGER NOT NULL);");
            try (PreparedStatement statement = connection.prepareStatement("INSERT INTO world VALUES (?, ?);")) {
                for (World world : worlds) {
                    statement.setInt(1, world.getId());
                    statement.setInt(2, world.getRandomNumber());
                    statement.addBatch();
                }
                statement.executeBatch();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public World findById(int id) {
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM world WHERE id = ?")) {
                statement.setInt(1, id);
                try (ResultSet resultSet = statement.executeQuery()) {
                    if (resultSet.next()) {
                        return new World(resultSet.getInt(1), resultSet.getInt(2));
                    }
                    throw new IllegalStateException("World with id: " + id + " not found!");
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> findByIds(List<Integer> ids) {
        List<World> worlds = new ArrayList<>(ids.size());
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                for (Integer randomNumber : ids) {
                    statement.setInt(1, randomNumber);
                    try (ResultSet resultSet = statement.executeQuery()) {
                        if (resultSet.next()) {
                            worlds.add(new World(resultSet.getInt(1), resultSet.getInt(2)));
                            continue;
                        }
                        throw new IllegalStateException("World with id: " + randomNumber + " not found!");
                    }
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return worlds;
    }

    @Override
    public void updateAll(Collection<World> worlds) {
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?")) {
                for (World world : worlds) {
                    statement.setInt(1, world.getRandomNumber());
                    statement.setInt(2, world.getId());
                    statement.addBatch();
                }
                statement.executeBatch();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

}
