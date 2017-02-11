package com.networknt.techempower.handler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.cache.LoadingCache;
import com.networknt.config.Config;
import com.networknt.techempower.model.World;
import com.networknt.techempower.Helper;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

public class CacheGetHandler implements HttpHandler {
    private final ObjectMapper objectMapper = Config.getInstance().getMapper();
    private final LoadingCache<Integer, World> worldCache = null;

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        int queries = Helper.getQueries(exchange);
        World[] worlds = new World[queries];
        for (int i = 0; i < queries; i++) {
            worlds[i] = worldCache.get(Helper.randomWorld());
        }
        exchange.getResponseHeaders().put(
                Headers.CONTENT_TYPE, "application/json");
        exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
    }
}
