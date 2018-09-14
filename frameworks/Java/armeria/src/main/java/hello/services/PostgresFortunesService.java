package hello.services;

import hello.models.Fortune;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.fasterxml.jackson.databind.ObjectMapper;

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

public class PostgresFortunesService
{
  private static final ObjectMapper MAPPER         = new ObjectMapper();
  private static final DateFormat   DATE_FORMATTER = new SimpleDateFormat(
      "E, dd MMM yyyy HH:mm:ss z");

  private static final String DATABASE_HOST     = "jdbc:postgresql://tfb-database:5432/hello_world";
  private static final String DATABASE_USER     = "benchmarkdbuser";
  private static final String DATABASE_PASSWORD = "benchmarkdbpass";

  private static final String SELECT_QUERY = "SELECT * FROM fortune";

  private final MustacheFactory mustacheFactory = new DefaultMustacheFactory();

  @Get("/fortunes") public HttpResponse fortunes()
  {
    List<Fortune> fortunes = getFortunes();
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    try
    {
      HttpHeaders headers = HttpHeaders.of(HttpStatus.OK).add(
          HttpHeaderNames.SERVER, "armeria").add(HttpHeaderNames.DATE,
          DATE_FORMATTER.format(new Date())).contentType(
          MediaType.HTML_UTF_8);

      return HttpResponse.of(headers,
          HttpData.ofUtf8(buildMustacheTemplate(fortunes)));
    }
    catch (Exception e)
    {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  private List<Fortune> getFortunes()
  {
    List<Fortune> fortunes = new ArrayList<>();
    try (Connection connection = DriverManager.getConnection(DATABASE_HOST,
        DATABASE_USER, DATABASE_PASSWORD))
    {
      final PreparedStatement statement = connection.prepareStatement(
          SELECT_QUERY);
      ResultSet resultSet = statement.executeQuery();
      while (resultSet.next())
      {
        fortunes.add(
            new Fortune(resultSet.getInt(1), resultSet.getString(2)));
      }
    }
    catch (SQLException e)
    {
    }
    return fortunes;
  }

  private String buildMustacheTemplate(List<Fortune> fortunes)
      throws IOException
  {
    // for some reason cannot access mustache template with file name.
    // manually getting the file and converting to a stream
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(
        classLoader.getResource("fortunes.mustache").getFile());
    Mustache mustache = mustacheFactory.compile(
        new InputStreamReader(new FileInputStream(file),
            Charset.forName("UTF-8")), "fortunes.mustache");
    StringWriter stringWriter = new StringWriter();

    mustache.execute(stringWriter, fortunes).flush();

    return stringWriter.toString();
  }
}
