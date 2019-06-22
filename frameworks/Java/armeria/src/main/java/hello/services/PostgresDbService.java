package hello.services;

import hello.helpers.HttpHeadersHelper;
import hello.helpers.PostgresDbHelper;
import hello.models.World;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Default;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.Param;
import com.zaxxer.hikari.HikariDataSource;

public class PostgresDbService {
  private static final ObjectMapper MAPPER = new ObjectMapper();

  private static final String SELECT_QUERY = "SELECT * FROM world WHERE id = ?";
  private static final String UPDATE_QUERY =
      "UPDATE world SET randomNumber = ? WHERE id = ?";

  private DataSource dataSource;

  public PostgresDbService() {
    dataSource = new HikariDataSource(PostgresDbHelper.hikariConfig());
  }

  @Get("/db")
  public HttpResponse db() throws Exception {
    return HttpResponse.of(
    	HttpHeadersHelper.getHttpHeader(MediaType.JSON),
        HttpData.of(MAPPER.writeValueAsBytes(getWorld(getRandomNumber())))
        );
  }

  // need to use regex as /queries/{count} doesn't work when count is null
  @Get("regex:^/queries/(?<count>.*)$")
  public HttpResponse queries(
      @Param("count")
      @Default("")
          String count) throws JsonProcessingException, SQLException {
    return HttpResponse.of(
        HttpHeadersHelper.getHttpHeader(MediaType.JSON),
        HttpData.of(
            MAPPER.writeValueAsBytes(getWorlds(getSanitizedCount(count))))
        );
  }

  @Get("regex:^/updates/(?<count>.*)$")
  public HttpResponse update(
      @Param("count")
      @Default("")
          String count) throws JsonProcessingException, SQLException {
    return HttpResponse.of(
        HttpHeadersHelper.getHttpHeader(MediaType.JSON),
        HttpData.of(
            MAPPER.writeValueAsBytes(
                getUpdatedWorlds(getSanitizedCount(count))))
        );
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

  private World getWorld(int number) throws SQLException {
    try (final Connection connection = dataSource.getConnection();
         final PreparedStatement statement =
             connection.prepareStatement(SELECT_QUERY)) {

      statement.setInt(1, number);

      try (final ResultSet resultSet = statement.executeQuery()) {
        resultSet.next();
        return new World(resultSet.getInt(1), resultSet.getInt(2));
      }
    }
  }

  private World[] getWorlds(int count) throws SQLException {
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
    }
    return worlds;
  }

  private World[] getUpdatedWorlds(int count) throws SQLException {
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
    }
    return worlds;
  }
}
