package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.Objects;

import static hello.HelloWebServer.JSON_UTF8;

/**
 * Handles the JSON test.
 */
final class JsonHandler implements HttpHandler {
  private final ObjectMapper objectMapper;

  public JsonHandler(ObjectMapper objectMapper) {
    this.objectMapper = Objects.requireNonNull(objectMapper);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    exchange.getResponseSender().send(ByteBuffer.wrap(
            objectMapper.writeValueAsBytes(
                    Collections.singletonMap("message", "Hello, World!"))));
  }
}
