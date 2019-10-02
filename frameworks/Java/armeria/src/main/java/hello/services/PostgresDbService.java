package hello.services;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import com.zaxxer.hikari.HikariDataSource;

import com.linecorp.armeria.server.ServiceRequestContext;
import com.linecorp.armeria.server.annotation.Default;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.Param;
import com.linecorp.armeria.server.annotation.ProducesJson;

import hello.helpers.PostgresDbHelper;
import hello.models.World;

public class PostgresDbService {

  private static final String SELECT_QUERY = "SELECT * FROM world WHERE id = ?";
  private static final String UPDATE_QUERY =
      "UPDATE world SET randomNumber = ? WHERE id = ?";

  private DataSource dataSource;

  public PostgresDbService() {
    dataSource = new HikariDataSource(PostgresDbHelper.hikariConfig());
  }

  @Get("/db")
  @ProducesJson
  public CompletableFuture<World> db() throws Exception {
    return getWorld(getRandomNumber());
  }

  // need to use regex as /queries/{count} doesn't work when count is null
  @Get("regex:^/queries/(?<count>.*)$")
  @ProducesJson
  public CompletableFuture<World[]> queries(
      @Param("count")
      @Default("") String count) {
    return getWorlds(getSanitizedCount(count));
  }

  @Get("regex:^/updates/(?<count>.*)$")
  @ProducesJson
  public CompletableFuture<World[]> update(
      @Param("count")
      @Default("") String count) {
    return getUpdatedWorlds(getSanitizedCount(count));
  }

  private static int getRandomNumber() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  private int getSanitizedCount(String count) {
    try {
      int intCount = Integer.parseInt(count);
      if (intCount < 1) {
        return 1;
      }
      if (intCount > 500) {
        return 500;
      }
      return intCount;
    } catch (NumberFormatException e) {
      return 1;
    }
  }

  private CompletableFuture<World> getWorld(int number) {
    return CompletableFuture.supplyAsync(
        () -> {
          try (final Connection connection = dataSource.getConnection();
               final PreparedStatement statement =
                   connection.prepareStatement(SELECT_QUERY)) {

            statement.setInt(1, number);

            try (final ResultSet resultSet = statement.executeQuery()) {
              resultSet.next();
              return new World(resultSet.getInt(1), resultSet.getInt(2));
            }
          } catch (SQLException e) {
            throw new IllegalStateException("Database error", e);
          }
        }, ServiceRequestContext.current().blockingTaskExecutor());
  }

  private CompletableFuture<World[]> getWorlds(int count) {
    return CompletableFuture.supplyAsync(
        () -> {
          World[] worlds = new World[count];

          try (final Connection connection = dataSource.getConnection()) {
            for (int i = 0; i < count; i++) {
              final int id = getRandomNumber();

              try (final PreparedStatement statement =
                       connection.prepareStatement(SELECT_QUERY)) {
                statement.setInt(1, id);

                try (final ResultSet resultSet = statement.executeQuery()) {
                  resultSet.next();
                  worlds[i] = new World(id, resultSet.getInt(2));
                }
              }
            }
          } catch (SQLException e) {
            throw new IllegalStateException("Database error", e);
          }
          return worlds;
        }, ServiceRequestContext.current().blockingTaskExecutor());
  }

  private CompletableFuture<World[]> getUpdatedWorlds(int count) {
    return CompletableFuture.supplyAsync(
        () -> {

          World[] worlds = new World[count];

          try (final Connection connection = dataSource.getConnection()) {
            for (int i = 0; i < count; i++) {
              final int id = getRandomNumber();
              final int randomNumber = getRandomNumber();

              try (final PreparedStatement select =
                       connection.prepareStatement(SELECT_QUERY);
                   final PreparedStatement update =
                       connection.prepareStatement(UPDATE_QUERY)) {

                // get
                select.setInt(1, id);

                try (final ResultSet set = select.executeQuery()) {
                  set.next();

                  // update
                  update.setInt(1, randomNumber);
                  update.setInt(2, id);
                  update.execute();

                  worlds[i] = new World(id, set.getInt(2));
                  worlds[i].randomNumber = randomNumber;
                }
              }
            }
          } catch (SQLException e) {
            throw new IllegalStateException("Database error", e);
          }
          return worlds;
        }, ServiceRequestContext.current().blockingTaskExecutor());
  }
}
