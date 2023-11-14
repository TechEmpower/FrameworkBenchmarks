package com.techempower.benchmark.pippo;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import com.techempower.benchmark.pippo.benchmark.Benchmark;
import com.techempower.benchmark.pippo.handler.Test6PlainTextHandler;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import ro.pippo.core.AbstractWebServer;
import ro.pippo.core.HttpConstants;
import ro.pippo.jetty.JettyServer;
import ro.pippo.tomcat.TomcatServer;
import ro.pippo.undertow.UndertowServer;

import static ro.pippo.core.HttpConstants.Header.CONTENT_TYPE;

@RunWith(Parameterized.class)
public class BenchmarkTests {

	@Parameterized.Parameters
	public static Collection<Object[]> data() {
		return Arrays.asList(new Object[][] {
			{ new JettyServer() },
			{ new UndertowServer() },
			{ new TomcatServer() },	// keep it as last parameter, since Tomcat doesn't stop gracefully and locks port 8080
		});
	}

	@Parameterized.BeforeParam
	public static void beforeParameter(AbstractWebServer<?> server) throws InterruptedException {

		benchmark = new Benchmark()
						.server(server)
						.serverName(server.getClass().getSimpleName())
						.start();

		client = new OkHttpClient();

		Thread.sleep(2000);

	}

	@Parameterized.AfterParam
	public static void afterParameter() {

		client.connectionPool().evictAll();

		benchmark.stop();

	}

	@Test
	public void test1() throws IOException {
		assertJson(execute("/json"));
	}

	@Test
	public void test2Postgres() throws IOException { test2(DbPostgres); }
	@Test
	public void test2Mysql() throws IOException { test2(DbMysql); }
	@Test
	public void test2Mongo() throws IOException { test2(DbMongo); }

	private void test2(String db) throws IOException {
		assertJson(execute(String.format("/%s/db", db)));
	}

	@Test
	public void test3Postgres() throws IOException { test3(DbPostgres); }
	@Test
	public void test3Mysql() throws IOException { test3(DbMysql); }
	@Test
	public void test3Mongo() throws IOException { test3(DbMongo); }

	private void test3(String db) throws IOException {
		assertJson(execute(String.format("/%s/queries", db)));
		assertJson(execute(String.format("/%s/queries?queries=-1", db)));
		assertJson(execute(String.format("/%s/queries?queries=ABC", db)));
		assertJson(execute(String.format("/%s/queries?queries=600", db)));
	}

	@Test
	public void test4Postgres() throws IOException { test4(DbPostgres); }
	@Test
	public void test4Mysql() throws IOException { test4(DbMysql); }
	@Test
	public void test4Mongo() throws IOException { test4(DbMongo); }

	private void test4(String db) throws IOException {
		assertHtml(execute(String.format("/%s/fortunes", db)));
		assertHtml(execute(String.format("/%s/fortunes", db)));
		assertHtml(execute(String.format("/%s/fortunes", db)));
	}

	@Test
	public void test5Postgres() throws IOException { test5(DbPostgres); }
	@Test
	public void test5Mysql() throws IOException { test5(DbMysql); }
	@Test
	public void test5Mongo() throws IOException { test5(DbMongo); }

	private void test5(String db) throws IOException {
		assertJson(execute(String.format("/%s/updates", db)));
		assertJson(execute(String.format("/%s/updates?queries=-1", db)));
		assertJson(execute(String.format("/%s/updates?queries=ABC", db)));
		assertJson(execute(String.format("/%s/updates?queries=600", db)));
	}

	@Test
	public void test6() throws IOException {
		assertHelloWorld(execute("/plaintext"));
	}

	private Response execute(String uri) throws IOException {
		Request request = new Request.Builder()
							  .url("http://localhost:8080" + uri)
							  .build();
		return client.newCall(request).execute();
	}

	private void assertHelloWorld(Response response) throws IOException {
		Assert.assertEquals("HTTP response code error", 200, response.code());
		Assert.assertTrue("Wrong content type: " + response.header(CONTENT_TYPE), response.header(CONTENT_TYPE).contains(HttpConstants.ContentType.TEXT_PLAIN));
		Assert.assertFalse("'Server' HTTP response header missing", StringUtils.isBlank(response.header(BenchmarkUtils.Header.SERVER)));
		Assert.assertFalse("'Date' HTTP response header missing", StringUtils.isBlank(response.header(HttpConstants.Header.DATE)));
		Assert.assertEquals(Test6PlainTextHandler.Message, response.body().string());
		response.close();
	}

	private void assertJson(Response response) {
		Assert.assertEquals("HTTP response code error", 200, response.code());
		Assert.assertTrue("Wrong content type: " + response.header(CONTENT_TYPE), response.header(CONTENT_TYPE).contains(HttpConstants.ContentType.APPLICATION_JSON));
		Assert.assertFalse("'Server' HTTP response header missing", StringUtils.isBlank(response.header(BenchmarkUtils.Header.SERVER)));
		Assert.assertFalse("'Date' HTTP response header missing", StringUtils.isBlank(response.header(HttpConstants.Header.DATE)));
		response.close();
	}

	private void assertHtml(Response response) {
		Assert.assertEquals("HTTP response code error", 200, response.code());
		Assert.assertTrue("Wrong content type: " + response.header(CONTENT_TYPE), response.header(CONTENT_TYPE).contains(HttpConstants.ContentType.TEXT_HTML));
		Assert.assertFalse("'Server' HTTP response header missing", StringUtils.isBlank(response.header(BenchmarkUtils.Header.SERVER)));
		Assert.assertFalse("'Date' HTTP response header missing", StringUtils.isBlank(response.header(HttpConstants.Header.DATE)));
		response.close();
	}

	private static final String DbPostgres = "postgres";
	private static final String DbMysql = "mysql";
	private static final String DbMongo = "mongo";

	@Parameterized.Parameter()
	public static AbstractWebServer<?> server;

	private static Benchmark benchmark;
	private static OkHttpClient client;

}