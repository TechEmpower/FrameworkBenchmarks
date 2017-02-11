package com.networknt.techempower.handler;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.config.Config;
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

public class DbMysqlGetHandler implements HttpHandler {
    private final DataSource ds = MysqlStartupHookProvider.ds;
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(25000);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        /*
        // throughput 8670 latency 122
        World[] worlds = new World[1];

        try (final Connection connection = ds.getConnection()) {
            Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
            futureWorlds.put(0, Helper.EXECUTOR.submit(new Callable<World>(){
                @Override
                public World call() throws Exception {
                    try (PreparedStatement statement = connection.prepareStatement(
                            "SELECT * FROM world WHERE id = ?",
                            ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

                        statement.setInt(1, Helper.randomWorld());
                        ResultSet resultSet = statement.executeQuery();
                        resultSet.next();
                        return new World(
                                resultSet.getInt("id"),
                                resultSet.getInt("randomNumber"));
                    }
                }
            }));

            worlds[0] = futureWorlds.get(0).get();
        }

        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, Helper.JSON_UTF8);
        exchange.getResponseSender().send(mapper.writeValueAsString(worlds[0]));
        */

        /*
        // throughput 11124 latency 137
        World world;
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

                statement.setInt(1, Helper.randomWorld());
                ResultSet resultSet = statement.executeQuery();
                resultSet.next();
                world = new World(
                        resultSet.getInt("id"),
                        resultSet.getInt("randomNumber"));
            }
        }
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, Helper.JSON_UTF8);
        exchange.getResponseSender().send(mapper.writeValueAsString(world));
        */
        /*
        // throughput 11680 latency 111
        String s;
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

                statement.setInt(1, Helper.randomWorld());
                ResultSet resultSet = statement.executeQuery();
                resultSet.next();
                s = "{\"id\":" + resultSet.getInt("id") + ",\"randomNumber\":" + resultSet.getInt("randomNumber") + "}";
            }
        }
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, Helper.JSON_UTF8);
        exchange.getResponseSender().send(s);
        */

        //11731 115
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
        World world;
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                statement.setInt(1, Helper.randomWorld());
                try(ResultSet resultSet = statement.executeQuery()) {
                    resultSet.next();
                    world = new World(resultSet.getInt("id"), resultSet.getInt("randomNumber"));
                }
            }
        }
        writer.reset();
        world.serialize(writer, true);
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.toByteArray()));
    }
}
