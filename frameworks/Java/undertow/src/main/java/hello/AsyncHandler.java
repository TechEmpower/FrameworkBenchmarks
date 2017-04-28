package hello;

import static hello.Helper.sendException;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.SameThreadExecutor;
import java.util.Objects;

/**
 * An HTTP handler that <em>does not</em> end the exchange when the call stack
 * of its {@link HttpHandler#handleRequest(HttpServerExchange)} method returns.
 * The handler must ensure that every exchange is ended through other means.
 */
final class AsyncHandler implements HttpHandler {
  private final HttpHandler handler;

  AsyncHandler(HttpHandler handler) {
    this.handler = Objects.requireNonNull(handler);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    Runnable asyncTask =
        () -> {
          try {
            handler.handleRequest(exchange);
          } catch (Exception e) {
            sendException(exchange, e);
          }
        };
    exchange.dispatch(SameThreadExecutor.INSTANCE, asyncTask);
  }
}
