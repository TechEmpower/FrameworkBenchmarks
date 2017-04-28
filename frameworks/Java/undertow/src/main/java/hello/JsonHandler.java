package hello;

import static hello.Helper.sendJson;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.util.Collections;
import java.util.Map;

/**
 * Handles the JSON test.
 */
final class JsonHandler implements HttpHandler {
  @Override
  public void handleRequest(HttpServerExchange exchange) {
    Map<String, String> value =
        Collections.singletonMap("message", "Hello, World!");
    sendJson(exchange, value);
  }
}
