package ro.pippo.benchmark.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import org.apache.commons.dbcp2.BasicDataSource;
import ro.pippo.benchmark.model.World;
import ro.pippo.benchmark.app.BenchmarkUtils;
import ro.pippo.benchmark.model.Fortune;
import ro.pippo.core.PippoSettings;

public class SqlDao implements Dao {

  private static final String READ_RANDOM_WORLD =
      "SELECT id, randomnumber FROM hello_world.World WHERE id = ?";

  private static final String UPDATE_RANDOM_WORLD =
      "UPDATE hello_world.World SET randomnumber = ? WHERE id = ?";

  private static final String READ_FORTUNES =
      "SELECT id, message FROM hello_world.Fortune";

  private final BasicDataSource dataSource;

  public SqlDao(PippoSettings settings, String keyPreffix) {
    dataSource = new BasicDataSource();
    dataSource.setDriverClassName(settings.getRequiredString(keyPreffix + ".driver"));
    dataSource.setUrl(settings.getRequiredString(keyPreffix + ".url"));
    dataSource.setUsername(settings.getRequiredString(keyPreffix + ".username"));
    dataSource.setPassword(settings.getRequiredString(keyPreffix + ".password"));

    // TODO verify optimal parameters for the pool
    dataSource.setMinIdle(settings.getInteger(keyPreffix + ".connection.min", 0));
    dataSource.setMaxIdle(settings.getInteger(keyPreffix + ".connection.max", 0));
  }

  @Override public World getRandomWorld() {
    try (
        Connection connection = dataSource.getConnection();
        PreparedStatement statement = connection.prepareStatement(READ_RANDOM_WORLD)
    ) {
      statement.setInt(1, BenchmarkUtils.random());
      ResultSet resultSet = statement.executeQuery();
      resultSet.next();
      int id = resultSet.getInt(1);
      int randomNumber = resultSet.getInt(2);
      return new World(id, randomNumber);
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }

  @Override public void updateRandomWorlds(List<World> worlds) {
    try (
        Connection connection = dataSource.getConnection();
        PreparedStatement statement = connection.prepareStatement(UPDATE_RANDOM_WORLD);
    ) {
      for (World world : worlds) {
        statement.setInt(1, world.randomNumber);
        statement.setInt(2, world.id);
        statement.addBatch();
      }
      statement.executeBatch();
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }

  @Override public List<Fortune> getFortunes() {
    try (
        Connection connection = dataSource.getConnection();
        PreparedStatement statement = connection.prepareStatement(READ_FORTUNES);
    ) {
      ResultSet resultSet = statement.executeQuery();
      List<Fortune> fortunes = new LinkedList<>();
      while (resultSet.next()) {
        int id = resultSet.getInt(1);
        String message = resultSet.getString(2);
        fortunes.add(new Fortune(id, message));
      }
      return fortunes;
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }
}
