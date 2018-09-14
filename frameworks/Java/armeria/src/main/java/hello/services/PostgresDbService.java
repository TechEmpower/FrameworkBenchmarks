package hello.services;

import hello.models.World;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.common.HttpHeaders;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Default;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.Param;
import com.linecorp.armeria.server.annotation.ProducesJson;

public class PostgresDbService
{
  private static final ObjectMapper MAPPER = new ObjectMapper();

  private static final DateFormat DATE_FORMATTER = new SimpleDateFormat(
      "E, dd MMM yyyy HH:mm:ss z");

  private static final String DATABASE_HOST     = "jdbc:postgresql://tfb-database:5432/hello_world";
  private static final String DATABASE_USER     = "benchmarkdbuser";
  private static final String DATABASE_PASSWORD = "benchmarkdbpass";

  private static final int    NUMBER_OF_RECORDS = 10000;
  private static final String SELECT_QUERY      = "SELECT * FROM world WHERE id = ?";
  private static final String UPDATE_QUERY      = "UPDATE world SET randomNumber = ? WHERE id = ?";

  @Get("/db") @ProducesJson public HttpResponse db()
  {
    try
    {
      HttpHeaders headers = HttpHeaders.of(HttpStatus.OK).add(
          HttpHeaderNames.SERVER, "armeria").add(HttpHeaderNames.DATE,
          DATE_FORMATTER.format(new Date())).contentType(
          MediaType.JSON_UTF_8);

      return HttpResponse.of(headers, HttpData.of(MAPPER.writeValueAsBytes(
          getWorld(getRandomNumber(1, NUMBER_OF_RECORDS)))));
    }
    catch (Exception e)
    {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  // need to use regex as /queries/{count} doesnt work when count is null
  @Get("regex:^/queries/(?<count>.*)$") @ProducesJson public HttpResponse queries(
      @Param("count") @Default("") String count)
  {
    try
    {
      HttpHeaders headers = HttpHeaders.of(HttpStatus.OK).add(
          HttpHeaderNames.SERVER, "armeria").add(HttpHeaderNames.DATE,
          DATE_FORMATTER.format(new Date())).contentType(
          MediaType.JSON_UTF_8);

      return HttpResponse.of(headers, HttpData.of(
          MAPPER.writeValueAsBytes(getWorlds(getSanitizedCount(count)))));
    }
    catch (JsonProcessingException e)
    {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @Get("regex:^/updates/(?<count>.*)$") @ProducesJson public HttpResponse update(
      @Param("count") @Default("") String count)
  {
    try
    {
      HttpHeaders headers = HttpHeaders.of(HttpStatus.OK).add(
          HttpHeaderNames.SERVER, "armeria").add(HttpHeaderNames.DATE,
          DATE_FORMATTER.format(new Date())).contentType(
          MediaType.JSON_UTF_8);

      return HttpResponse.of(headers, HttpData.of(MAPPER.writeValueAsBytes(
          getUpdatedWorlds(getSanitizedCount(count)))));

    }
    catch (JsonProcessingException e)
    {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  private int getRandomNumber(int min, int max)
  {
    Random r = new Random();
    return r.nextInt(max - min) + min;
  }

  private int getSanitizedCount(String count)
  {
    try
    {
      int intCount = Integer.parseInt(count);
      if (intCount < 1)
        return 1;
      if (intCount > 500)
        return 500;
      return intCount;
    }
    catch (NumberFormatException e)
    {
      return 1;
    }
  }

  private World getWorld(int number)
  {
    try (Connection connection = DriverManager.getConnection(DATABASE_HOST,
        DATABASE_USER, DATABASE_PASSWORD))
    {
      final PreparedStatement statement = connection.prepareStatement(
          SELECT_QUERY);
      statement.setInt(1, number);
      ResultSet resultSet = statement.executeQuery();
      resultSet.next();
      return new World(resultSet.getInt(1), resultSet.getInt(2));
    }
    catch (SQLException e)
    {
    }

    return null;
  }

  private World[] getWorlds(int count)
  {
    World[] worlds = new World[count];

    try (Connection connection = DriverManager.getConnection(DATABASE_HOST,
        DATABASE_USER, DATABASE_PASSWORD))
    {
      for (int i = 0; i < count; i++)
      {
        final int id = getRandomNumber(1, NUMBER_OF_RECORDS);
        final PreparedStatement statement = connection.prepareStatement(
            SELECT_QUERY);

        statement.setInt(1, id);
        ResultSet resultSet = statement.executeQuery();
        resultSet.next();

        worlds[i] = new World(id, resultSet.getInt(2));
      }
    }
    catch (SQLException e)
    {
    }

    return worlds;
  }

  private World[] getUpdatedWorlds(int count)
  {
    World[] worlds = new World[count];

    try (Connection connection = DriverManager.getConnection(DATABASE_HOST,
        DATABASE_USER, DATABASE_PASSWORD))
    {
      for (int i = 0; i < count; i++)
      {
        final int id = getRandomNumber(1, NUMBER_OF_RECORDS);
        final int randomNumber = getRandomNumber(1, 10000);

        final PreparedStatement select = connection.prepareStatement(
            SELECT_QUERY);
        final PreparedStatement update = connection.prepareStatement(
            UPDATE_QUERY);

        // get
        select.setInt(1, id);
        ResultSet set = select.executeQuery();
        set.next();
        worlds[i] = new World(id, set.getInt(2));

        // update
        update.setInt(1, randomNumber);
        update.setInt(2, id);
        update.execute();

        worlds[i].randomNumber = randomNumber;
      }
    }
    catch (SQLException e)
    {
    }

    return worlds;
  }
}
