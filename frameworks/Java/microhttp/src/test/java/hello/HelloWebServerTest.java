package hello;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

class HelloWebServerTest {

    @Test
    void plainTextAndJson() throws IOException, InterruptedException {
        HelloWebServer server = new HelloWebServer(8080);
        Runnable task = () -> {
            try {
                server.start();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        };
        Thread thread = new Thread(task);
        thread.setDaemon(true);
        thread.start();
        HttpClient client = HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_1_1)
                .build();
        verifyPlainText(client);
        verifyJson(client);
        verifyOther(client);
    }

    static void verifyPlainText(HttpClient client) throws IOException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder()
                .GET()
                .uri(URI.create("http://localhost:8080/plaintext"))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        Assertions.assertEquals(200, response.statusCode());
        Assertions.assertEquals("Hello, World!", response.body());
    }

    static void verifyJson(HttpClient client) throws IOException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder()
                .GET()
                .uri(URI.create("http://localhost:8080/json"))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        Assertions.assertEquals(200, response.statusCode());
        Assertions.assertEquals("{\"message\":\"Hello, World!\"}", response.body());
    }

    static void verifyOther(HttpClient client) throws IOException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder()
                .GET()
                .uri(URI.create("http://localhost:8080/unknown"))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        Assertions.assertEquals(404, response.statusCode());
    }

}
