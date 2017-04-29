package hello;

import static io.undertow.util.Headers.CONTENT_TYPE;
import static java.nio.charset.StandardCharsets.US_ASCII;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.nio.ByteBuffer;

/**
 * Handles the plaintext test.
 */
final class PlaintextHandler implements HttpHandler {
  @Override
  public void handleRequest(HttpServerExchange exchange) {
    exchange.getResponseHeaders().put(CONTENT_TYPE, "text/plain");
    exchange.getResponseSender().send(buffer.duplicate());
  }

  // Normally, one would send the string "Hello, World!" directly.  Reusing a
  // ByteBuffer is a micro-optimization that is explicitly permitted by the
  // plaintext test requirements.

  private static final ByteBuffer buffer;
  static {
    String message = "Hello, World!";
    byte[] messageBytes = message.getBytes(US_ASCII);
    buffer = ByteBuffer.allocateDirect(messageBytes.length);
    buffer.put(messageBytes);
    buffer.flip();
  }
}
