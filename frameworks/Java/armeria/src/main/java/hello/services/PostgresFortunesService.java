package hello.services;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import javax.sql.DataSource;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.zaxxer.hikari.HikariDataSource;

import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.ServiceRequestContext;
import com.linecorp.armeria.server.annotation.Get;

import hello.helpers.PostgresDbHelper;
import hello.models.Fortune;

public class PostgresFortunesService {
  private static final String SELECT_QUERY = "SELECT * FROM fortune";

  // Mustache cannot find a classloader on the current thread. Have to pass the classloader manually.
  // TODO: Look into why mustache cannot find a classloader.
  private final MustacheFactory mustacheFactory = new DefaultMustacheFactory(
      name -> new InputStreamReader(
          HelloService.class.getClassLoader().getResourceAsStream(name)));

  private DataSource dataSource;

  public PostgresFortunesService() {
    dataSource = new HikariDataSource(PostgresDbHelper.hikariConfig());
  }

  @Get("/fortunes")
  public CompletableFuture<HttpResponse> fortunes(ServiceRequestContext ctx) {
    return getFortunes(ctx).thenApply(fortunes -> {
      fortunes.add(
          new Fortune(0, "Additional fortune added at request time."));
      fortunes.sort(Comparator.comparing(Fortune::getMessage));

      return HttpResponse.of(
              HttpStatus.OK, MediaType.HTML_UTF_8, buildMustacheTemplate(fortunes));
    });
  }

  private CompletableFuture<List<Fortune>> getFortunes(ServiceRequestContext ctx) {
    return CompletableFuture.supplyAsync(
        () -> {
          List<Fortune> fortunes = new ArrayList<>();

          try (final Connection connection = dataSource.getConnection();
               final PreparedStatement statement =
                   connection.prepareStatement(SELECT_QUERY);
               final ResultSet resultSet = statement.executeQuery()) {

            while (resultSet.next()) {
              fortunes.add(
                  new Fortune(
                      resultSet.getInt(1),
                      resultSet.getString(2)));
            }
          } catch (SQLException e) {
            throw new IllegalStateException("Database error", e);
          }
          return fortunes;
        }, ctx.blockingTaskExecutor()
    );
  }

  private byte[] buildMustacheTemplate(List<Fortune> fortunes) {
    Mustache mustache = mustacheFactory.compile("fortunes.mustache");
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();

    try (Writer writer = new OutputStreamWriter(bytes, StandardCharsets.UTF_8)) {
      mustache.execute(writer, fortunes);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    return bytes.toByteArray();
  }
}
