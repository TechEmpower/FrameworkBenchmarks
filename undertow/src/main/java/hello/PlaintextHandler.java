package hello;

import com.google.common.net.MediaType;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import static hello.HelloWebServer.TEXT_PLAIN;

/**
 * Handles the plaintext test.
 */
final class PlaintextHandler implements HttpHandler {
  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, TEXT_PLAIN);
    exchange.getResponseSender().send("Hello, World!");
  }
}
