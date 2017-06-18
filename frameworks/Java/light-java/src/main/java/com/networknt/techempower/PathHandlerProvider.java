package com.networknt.techempower;

import com.networknt.config.Config;
import com.networknt.server.HandlerProvider;
import io.undertow.Handlers;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Methods;
import com.networknt.techempower.handler.*;

public class PathHandlerProvider implements HandlerProvider {
    @Override
    public HttpHandler getHandler() {
        return Handlers.path()
            //.addExactPath("/db/mysql", new DbMysqlGetHandler())
            .addExactPath("/plaintext", new PlaintextGetHandler())
            .addExactPath("/json", new JsonGetHandler())
            .addExactPath("/db", new DbPostgresqlGetHandler())
            //.addExactPath("/fortunes/mysql", new FortunesMysqlGetHandler())
            .addExactPath("/fortunes", new FortunesPostgresqlGetHandler())
            //.addExactPath("/queries/mysql", new QueriesMysqlGetHandler())
            .addExactPath("/queries", new QueriesPostgresqlGetHandler())
            //.addExactPath("/updates/mysql", new UpdatesMysqlGetHandler())
            .addExactPath("/updates", new UpdatesPostgresqlGetHandler())
        ;
    }
}

