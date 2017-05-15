package com.networknt.techempower.handler;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.networknt.techempower.db.postgres.PostgresStartupHookProvider;
import com.networknt.techempower.model.Fortune;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.io.StringWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.*;

import javax.sql.DataSource;

public class FortunesPostgresqlGetHandler implements HttpHandler {
    private final MustacheFactory mustacheFactory = new DefaultMustacheFactory();
    private final DataSource ds = PostgresStartupHookProvider.ds;

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        if (exchange.isInIoThread()) {
            exchange.dispatch(this);
            return;
        }
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection connection = ds.getConnection();
             PreparedStatement statement = connection.prepareStatement(
                     "SELECT * FROM Fortune",
                     ResultSet.TYPE_FORWARD_ONLY,
                     ResultSet.CONCUR_READ_ONLY);
             ResultSet resultSet = statement.executeQuery()) {
            while (resultSet.next()) {
                fortunes.add(new Fortune(
                        resultSet.getInt("id"),
                        resultSet.getString("message")));
            }
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        Mustache mustache = mustacheFactory.compile("fortunes.mustache");
        StringWriter writer = new StringWriter();
        mustache.execute(writer, fortunes);
        exchange.getResponseHeaders().put(
                Headers.CONTENT_TYPE, "text/html");
        exchange.getResponseSender().send(writer.toString());
    }
}
