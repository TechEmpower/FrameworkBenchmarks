package benchmark;

import io.micronaut.http.HttpResponse;
import io.micronaut.http.client.HttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import jakarta.inject.Inject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

@MicronautTest(transactional = false, environments = {"common", "common-test"})
public class JsonSerializationTest {

    @Inject
    @Client("/")
    HttpClient httpClient;

    @Test
    public void test() {
        HttpResponse<String> response = httpClient.toBlocking().exchange("/json", String.class);
        Assertions.assertEquals("{\"message\":\"Hello, World!\"}", response.body());
        Assertions.assertEquals("Micronaut", response.header("Server"));
        Assertions.assertFalse(response.header("Date").isEmpty());
    }

}
