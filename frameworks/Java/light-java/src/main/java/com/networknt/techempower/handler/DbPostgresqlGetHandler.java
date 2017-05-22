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
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringEscapeUtils;

import javax.sql.DataSource;

public class DbPostgresqlGetHandler implements HttpHandler {
    private final DataSource ds = PostgresStartupHookProvider.ds;
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(512);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        // 24682 59
        World world;
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                statement.setInt(1, Helper.randomWorld());
                try (ResultSet resultSet = statement.executeQuery()) {
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
