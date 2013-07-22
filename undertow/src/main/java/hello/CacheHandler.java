package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.cache.LoadingCache;
import com.google.common.net.MediaType;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.util.Objects;

import static hello.HelloWebServer.JSON_UTF8;

/**
 * Handles the cache access test.
 */
final class CacheHandler implements HttpHandler {
  private final ObjectMapper objectMapper;
  private final LoadingCache<Integer, World> worldCache;

  CacheHandler(ObjectMapper objectMapper,
               LoadingCache<Integer, World> worldCache) {
    this.objectMapper = Objects.requireNonNull(objectMapper);
    this.worldCache = Objects.requireNonNull(worldCache);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    int queries = Helper.getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      worlds[i] = worldCache.get(Helper.randomWorld());
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }
}
