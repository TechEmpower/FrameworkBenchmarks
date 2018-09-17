package hello.services;

import hello.models.Fortune;

import java.io.InputStreamReader;
import java.io.IOException;
import java.io.StringWriter;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.common.HttpHeaders;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Get;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

public class PostgresFortunesService {
  private static final String DATABASE_HOST =
      "jdbc:postgresql://tfb-database:5432/hello_world";
  private static final String DATABASE_USER = "benchmarkdbuser";
  private static final String DATABASE_PASSWORD = "benchmarkdbpass";

  private static final String SELECT_QUERY = "SELECT * FROM fortune";

  // Mustache cannot find a classloader on the current thread. Have to pass the classloader manually.
  // TODO: Look into why mustache cannot find a classloader.
  private final MustacheFactory mustacheFactory = new DefaultMustacheFactory(
      name -> new InputStreamReader(
          HelloService.class.getClassLoader().getResourceAsStream(name)));

  @Get("/fortunes")
  public HttpResponse fortunes() {
    List<Fortune> fortunes = getFortunes();
    fortunes.add(
        new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    try {
      HttpHeaders headers = HttpHeaders
          .of(HttpStatus.OK)
          .add(HttpHeaderNames.SERVER, "armeria")
          .add(HttpHeaderNames.DATE,
               DateTimeFormatter.RFC_1123_DATE_TIME
                   .format(ZonedDateTime.now(ZoneOffset.UTC)))
          .contentType(MediaType.HTML_UTF_8);

      return HttpResponse.of(
          headers,
          HttpData.ofUtf8(buildMustacheTemplate(fortunes)));
    } catch (Exception e) {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  private List<Fortune> getFortunes() {
    List<Fortune> fortunes = new ArrayList<>();
    try (Connection connection = DriverManager.getConnection(
        DATABASE_HOST,
        DATABASE_USER,
        DATABASE_PASSWORD)) {
      final PreparedStatement statement = connection.prepareStatement(
          SELECT_QUERY);
      ResultSet resultSet = statement.executeQuery();

      while (resultSet.next()) {
        fortunes.add(
            new Fortune(
                resultSet.getInt(1),
                resultSet.getString(2)));
      }
    } catch (SQLException e) {}

    return fortunes;
  }

  private String buildMustacheTemplate(List<Fortune> fortunes)
      throws IOException {
    Mustache mustache = mustacheFactory.compile("fortunes.mustache");
    StringWriter stringWriter = new StringWriter();

    mustache.execute(stringWriter, fortunes).flush();

    return stringWriter.toString();
  }
}
