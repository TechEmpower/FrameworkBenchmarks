package com.networknt.techempower.handler;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.networknt.techempower.Helper;
import com.networknt.techempower.db.mysql.MysqlStartupHookProvider;
import com.networknt.techempower.model.World;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;

import java.nio.ByteBuffer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;

import org.apache.commons.lang3.StringEscapeUtils;

import javax.sql.DataSource;

public class UpdatesMysqlGetHandler implements HttpHandler {
    private final DataSource ds = MysqlStartupHookProvider.ds;
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(25000);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        int queries = Helper.getQueries(exchange);
        World[] worlds = new World[queries];
        try (final Connection connection = ds.getConnection()) {
            Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
            for (int i = 0; i < queries; i++) {
                futureWorlds.put(i, Helper.EXECUTOR.submit(new Callable<World>() {
                    @Override
                    public World call() throws Exception {
                        World world;
                        try (PreparedStatement update = connection.prepareStatement(
                                "UPDATE world SET randomNumber = ? WHERE id= ?")) {
                            try (PreparedStatement query = connection.prepareStatement(
                                    "SELECT * FROM world WHERE id = ?",
                                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

                                query.setInt(1, Helper.randomWorld());
                                try (ResultSet resultSet = query.executeQuery()) {
                                    resultSet.next();
                                    world = new World(
                                            resultSet.getInt("id"),
                                            resultSet.getInt("randomNumber"));
                                }
                            }
                            world.randomNumber = Helper.randomWorld();
                            update.setInt(1, world.randomNumber);
                            update.setInt(2, world.id);
                            update.executeUpdate();
                            return world;
                        }
                    }
                }));
            }
            for (int i = 0; i < queries; i++) {
                worlds[i] = futureWorlds.get(i).get();
            }
        }
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
        writer.reset();
        writer.serialize(worlds, queries);
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.toByteArray()));
    }
}
