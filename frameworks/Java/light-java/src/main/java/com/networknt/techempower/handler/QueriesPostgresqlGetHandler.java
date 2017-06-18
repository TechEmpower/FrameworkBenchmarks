package com.networknt.techempower.handler;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.config.Config;
import com.networknt.techempower.Helper;
import com.networknt.techempower.db.mysql.MysqlStartupHookProvider;
import com.networknt.techempower.db.postgres.PostgresStartupHookProvider;
import com.networknt.techempower.model.World;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;

import java.nio.ByteBuffer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.StringEscapeUtils;

import javax.sql.DataSource;

public class QueriesPostgresqlGetHandler implements HttpHandler {
    private final DataSource ds = PostgresStartupHookProvider.ds;
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(1024);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");

        int queries = Helper.getQueries(exchange);

        List<CompletableFuture<World>> worlds = IntStream.range(0, queries)
                .mapToObj(i -> CompletableFuture.supplyAsync(() -> Helper.selectWorld(ds), Helper.executor))
                .collect(Collectors.toList());

        CompletableFuture<List<World>> allDone = Helper.sequence(worlds);
        writer.reset();
        writer.serialize(allDone.get());
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.toByteArray()));
    }

}
