package ro.pippo.benchmark;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import ro.pippo.benchmark.model.HelloWorld;
import ro.pippo.core.AbstractWebServer;
import ro.pippo.jetty.JettyServer;
import ro.pippo.tjws.TjwsServer;
import ro.pippo.tomcat.TomcatServer;
import ro.pippo.undertow.UndertowServer;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static ro.pippo.benchmark.app.BenchmarkUtils.CONTENT_TYPE_HTML;
import static ro.pippo.benchmark.app.BenchmarkUtils.CONTENT_TYPE_JSON;
import static ro.pippo.benchmark.app.BenchmarkUtils.CONTENT_TYPE_TEXT_PLAIN;
import static ro.pippo.benchmark.app.BenchmarkUtils.HEADER_SERVER;
import static ro.pippo.benchmark.app.BenchmarkUtils.HEADER_SERVER_VALUE;
import static ro.pippo.core.HttpConstants.Header.CONTENT_TYPE;
import static ro.pippo.core.HttpConstants.Header.DATE;

@RunWith(Parameterized.class)
public class BenchmarkTests {

  @Parameterized.Parameters
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][] {
        { new JettyServer() },
        { new TjwsServer() },
        { new TomcatServer() },
        { new UndertowServer() },
    });
  }

  private AbstractWebServer server;
  private Benchmark benchmark;
  private OkHttpClient client;

  public BenchmarkTests(AbstractWebServer server) {
    this.server = server;
  }

  @Before
  public void startServer() throws InterruptedException {
    benchmark = new Benchmark()
        .server(server)
        .start();
    client = new OkHttpClient();

    Thread.sleep(2000);
  }

  @After
  public void stopServer() {
    benchmark.stop();
  }

  @Test
  public void tests() throws IOException {
    assertJson(execute("/json"));

    assertJson(execute("/mysql/db"));
    assertJson(execute("/psql/db"));
    assertJson(execute("/mongo/db"));

    assertJson(execute("/mysql/queries"));
    assertJson(execute("/mysql/queries?queries=-1"));
    assertJson(execute("/mysql/queries?queries=ABC"));
    assertJson(execute("/mysql/queries?queries=600"));
    assertJson(execute("/psql/queries"));
    assertJson(execute("/psql/queries?queries=-1"));
    assertJson(execute("/psql/queries?queries=ABC"));
    assertJson(execute("/psql/queries?queries=600"));
    assertJson(execute("/mongo/queries"));
    assertJson(execute("/mongo/queries?queries=-1"));
    assertJson(execute("/mongo/queries?queries=ABC"));
    assertJson(execute("/mongo/queries?queries=600"));

    assertHtml(execute("/mysql/fortunes"));
    assertHtml(execute("/psql/fortunes"));
    assertHtml(execute("/mongo/fortunes"));

    assertJson(execute("/mysql/updates"));
    assertJson(execute("/mysql/updates?queries=-1"));
    assertJson(execute("/mysql/updates?queries=ABC"));
    assertJson(execute("/mysql/updates?queries=600"));
    assertJson(execute("/psql/updates"));
    assertJson(execute("/psql/updates?queries=-1"));
    assertJson(execute("/psql/updates?queries=ABC"));
    assertJson(execute("/psql/updates?queries=600"));
    assertJson(execute("/mongo/updates"));
    assertJson(execute("/mongo/updates?queries=-1"));
    assertJson(execute("/mongo/updates?queries=ABC"));
    assertJson(execute("/mongo/updates?queries=600"));

    assertHelloWorld(execute("/plaintext"));
  }

  private Response execute(String uri) throws IOException {
    Request request = new Request.Builder()
        .url("http://localhost:8080" + uri)
        .build();

    return client.newCall(request).execute();
  }

  private void assertHelloWorld(Response response) throws IOException {
    assertEquals(200, response.code());
    assertTrue(response.header(CONTENT_TYPE).contains(CONTENT_TYPE_TEXT_PLAIN));
    assertTrue(response.header(HEADER_SERVER).contains(HEADER_SERVER_VALUE));
    assertNotNull(response.header(DATE));
    assertEquals(HelloWorld.MESSAGE, response.body().string());
  }

  private void assertJson(Response response) {
    assertEquals(200, response.code());
    assertTrue(response.header(CONTENT_TYPE).contains(CONTENT_TYPE_JSON));
    assertTrue(response.header(HEADER_SERVER).contains(HEADER_SERVER_VALUE));
    assertNotNull(response.header(DATE));
  }

  private void assertHtml(Response response) {
    assertEquals(200, response.code());
    assertTrue(response.header(CONTENT_TYPE).contains(CONTENT_TYPE_HTML));
    assertTrue(response.header(HEADER_SERVER).contains(HEADER_SERVER_VALUE));
    assertNotNull(response.header(DATE));
  }
}
