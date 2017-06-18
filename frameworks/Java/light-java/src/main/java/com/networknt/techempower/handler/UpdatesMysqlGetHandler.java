package com.networknt.techempower.handler;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.networknt.techempower.Helper;
import com.networknt.techempower.db.mysql.MysqlStartupHookProvider;
import com.networknt.techempower.model.World;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;

import javax.sql.DataSource;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class UpdatesMysqlGetHandler implements HttpHandler {
    private final DataSource ds = MysqlStartupHookProvider.ds;
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(1024);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        int queries = Helper.getQueries(exchange);

        List<CompletableFuture<World>> worlds = IntStream.range(0, queries)
                .mapToObj(i -> CompletableFuture.supplyAsync(() -> Helper.updateWorld(ds), Helper.executor))
                .collect(Collectors.toList());

        CompletableFuture<List<World>> allDone = Helper.sequence(worlds);
        writer.reset();
        writer.serialize(allDone.get());
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.toByteArray()));
    }
}
