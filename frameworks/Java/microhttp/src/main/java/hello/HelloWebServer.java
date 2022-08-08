package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.microhttp.EventLoop;
import org.microhttp.Header;
import org.microhttp.LogEntry;
import org.microhttp.Logger;
import org.microhttp.Options;
import org.microhttp.Request;
import org.microhttp.Response;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.function.Consumer;

public class HelloWebServer {

    static final String MESSAGE = "Hello, World!";
    static final byte[] TEXT_BYTES = MESSAGE.getBytes(StandardCharsets.UTF_8);

    static final String SERVER = "microhttp";

    static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC);

    static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    final int port;

    volatile String date = DATE_FORMATTER.format(Instant.now());

    record JsonMessage(String message) {
    }

    HelloWebServer(int port) {
        this.port = port;
    }

    void start() throws IOException, InterruptedException {
        startUpdater();
        Options options = new Options()
                .withHost(null) // wildcard any-address binding
                .withPort(port)
                .withReuseAddr(true)
                .withReusePort(true)
                .withAcceptLength(8_192)
                .withMaxRequestSize(1_024 * 1_024)
                .withReadBufferSize(1_024 * 64)
                .withResolution(Duration.ofMillis(1_000))
                .withRequestTimeout(Duration.ofSeconds(90));
        EventLoop eventLoop = new EventLoop(options, new DisabledLogger(), this::handle);
        eventLoop.start();
        eventLoop.join();
    }

    void startUpdater() {
        Thread thread = new Thread(this::runDateUpdater);
        thread.setDaemon(true);
        thread.setPriority(Thread.MIN_PRIORITY);
        thread.start();
    }

    void runDateUpdater() {
        while (true) {
            try {
                Thread.sleep(1_000);
            } catch (InterruptedException e) {
                return;
            }
            date = DATE_FORMATTER.format(Instant.now());
        }
    }

    void handle(Request request, Consumer<Response> callback) {
        if (request.uri().equals("/plaintext")) {
            List<Header> headers = List.of(
                    new Header("Content-Type", "text/plain"),
                    new Header("Date", date),
                    new Header("Server", SERVER));
            callback.accept(new Response(200, "OK", headers, TEXT_BYTES));
        } else if (request.uri().equals("/json")) {
            List<Header> headers = List.of(
                    new Header("Content-Type", "application/json"),
                    new Header("Date", date),
                    new Header("Server", SERVER));
            callback.accept(new Response(200, "OK", headers, jsonBody()));
        } else {
            List<Header> headers = List.of(
                    new Header("Date", date),
                    new Header("Server", SERVER));
            callback.accept(new Response(404, "Not Found", headers, new byte[0]));
        }
    }

    static byte[] jsonBody() {
        try {
            return OBJECT_MAPPER.writeValueAsBytes(new JsonMessage(MESSAGE));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        int port = args.length > 0
                ? Integer.parseInt(args[0])
                : 8080;
        new HelloWebServer(port).start();
    }

    static class DisabledLogger implements Logger {
        @Override
        public boolean enabled() {
            return false;
        }

        @Override
        public void log(LogEntry... logEntries) {

        }

        @Override
        public void log(Exception e, LogEntry... logEntries) {

        }
    }

}
