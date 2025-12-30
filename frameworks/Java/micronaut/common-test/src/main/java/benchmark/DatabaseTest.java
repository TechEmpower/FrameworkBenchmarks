package benchmark;

import io.micronaut.core.io.IOUtils;
import io.micronaut.core.type.Argument;
import io.micronaut.core.util.StringUtils;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.client.HttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import jakarta.inject.Inject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.List;
import java.util.Map;

@MicronautTest(transactional = false, environments = {"common", "common-test"})
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class DatabaseTest {

    @Inject
    @Client("/")
    HttpClient httpClient;

    @BeforeAll
    public void prepareData() {
        httpClient.toBlocking().exchange("/prepare-data-for-test");
    }

    @Test
    public void singleDatabaseQuery() {
        HttpResponse<Map<String, Object>> response = httpClient.toBlocking().exchange(HttpRequest.GET("/db"), Argument.mapOf(String.class, Object.class));
        Map<String, Object> world = response.body();
        Assertions.assertNotNull(world);
        Assertions.assertTrue(world.containsKey("id"));
        Assertions.assertTrue(world.containsKey("randomNumber"));
        Assertions.assertEquals("Micronaut", response.header("Server"));
        Assertions.assertEquals("application/json", response.header("Content-Type"));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Date")));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Content-Length")));
    }

    @Test
    public void multipleDatabaseQuery() {
        HttpResponse<List<Map<String, Object>>> response = httpClient.toBlocking().exchange(HttpRequest.GET("/queries?queries=10"), Argument.listOf(Argument.mapOf(String.class, Object.class)));
        List<Map<String, Object>> worlds = response.body();
        Assertions.assertNotNull(worlds);
        Assertions.assertEquals(worlds.size(), 10);
        for (Map<String, Object> world : worlds) {
            Assertions.assertTrue(world.containsKey("id"));
            Assertions.assertTrue(world.containsKey("randomNumber"));
        }
        Assertions.assertEquals("Micronaut", response.header("Server"));
        Assertions.assertEquals("application/json", response.header("Content-Type"));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Date")));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Content-Length")));
    }

    @Test
    public void fortunes() {
        HttpResponse<String> response = httpClient.toBlocking().exchange(HttpRequest.GET("/fortunes"), String.class);
        Assertions.assertEquals(getFortunesHtmlString(), response.body());
        Assertions.assertEquals("Micronaut", response.header("Server"));
        Assertions.assertEquals("text/html;charset=utf-8", response.header("Content-Type"));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Date")));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Content-Length")));
    }

    @Test
    public void databaseUpdates() {
        HttpResponse<List<Map<String, Object>>> response = httpClient.toBlocking().exchange(HttpRequest.GET("/updates?queries=10"), Argument.listOf(Argument.mapOf(String.class, Object.class)));
        List<Map<String, Object>> worlds = response.body();
        Assertions.assertNotNull(worlds);
        Assertions.assertEquals(worlds.size(), 10);
        for (Map<String, Object> world : worlds) {
            Assertions.assertTrue(world.containsKey("id"));
            Assertions.assertTrue(world.containsKey("randomNumber"));
        }
        Assertions.assertEquals("Micronaut", response.header("Server"));
        Assertions.assertEquals("application/json", response.header("Content-Type"));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Date")));
        Assertions.assertTrue(StringUtils.isNotEmpty(response.header("Content-Length")));
    }

    private String getFortunesHtmlString() {
        try (InputStream resourceAsStream = getClass().getResourceAsStream("/fortunes.html")) {
            return IOUtils.readText(new BufferedReader(new InputStreamReader(resourceAsStream)));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
