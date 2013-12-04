package hello;

import com.google.common.net.MediaType;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import java.nio.ByteBuffer;

import static hello.HelloWebServer.TEXT_PLAIN;

/**
 * Handles the plaintext test.
 */
final class PlaintextHandler implements HttpHandler {
  private static final ByteBuffer buffer;
  private static final String MESSAGE = "Hello, World!";

  static {
      buffer = ByteBuffer.allocateDirect(MESSAGE.length());   
      try {
          buffer.put(MESSAGE.getBytes("US-ASCII"));
      } catch (Exception e) {
          throw new RuntimeException(e);
      }
      buffer.flip();
  }
     
  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, TEXT_PLAIN);
    exchange.getResponseSender().send(buffer.duplicate());
  }
}
