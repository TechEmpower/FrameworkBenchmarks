package hello.services;

import hello.helpers.PostgresDbHelper;
import hello.models.Fortune;
import hello.helpers.HttpHeadersHelper;

import java.io.InputStreamReader;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Get;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

public class PostgresFortunesService {
  private static final String SELECT_QUERY = "SELECT * FROM fortune";

  // Mustache cannot find a classloader on the current thread. Have to pass the classloader manually.
  // TODO: Look into why mustache cannot find a classloader.
  private final MustacheFactory mustacheFactory = new DefaultMustacheFactory(
      name -> new InputStreamReader(
          HelloService.class.getClassLoader().getResourceAsStream(name)));

  @Get("/fortunes")
  public HttpResponse fortunes() throws SQLException, IOException {
    List<Fortune> fortunes = getFortunes();
    fortunes.add(
        new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    return HttpResponse.of(
        new HttpHeadersHelper().getHttpHeader(MediaType.HTML_UTF_8),
        HttpData.ofUtf8(buildMustacheTemplate(fortunes)));
  }

  private List<Fortune> getFortunes() throws SQLException {
    List<Fortune> fortunes = new ArrayList<>();

    Connection connection = DriverManager.getConnection(
        PostgresDbHelper.DATABASE_HOST,
        PostgresDbHelper.DATABASE_USER,
        PostgresDbHelper.DATABASE_PASSWORD);

    final PreparedStatement statement =
        connection.prepareStatement(SELECT_QUERY);
    ResultSet resultSet = statement.executeQuery();

    while (resultSet.next()) {
      fortunes.add(
          new Fortune(
              resultSet.getInt(1),
              resultSet.getString(2)));
    }

    connection.close();
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
