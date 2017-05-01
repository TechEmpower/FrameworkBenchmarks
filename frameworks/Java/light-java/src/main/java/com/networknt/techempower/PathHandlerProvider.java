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
            .addPrefixPath("/db/mysql", new DbMysqlGetHandler())
            .addPrefixPath("/db/postgresql", new DbPostgresqlGetHandler())
            .addPrefixPath("/fortunes/mysql", new FortunesMysqlGetHandler())
            .addPrefixPath("/fortunes/postgresql", new FortunesPostgresqlGetHandler())
            .addPrefixPath("/json", new JsonGetHandler())
            .addPrefixPath("/plaintext", new PlaintextGetHandler())
            .addPrefixPath("/queries/mysql", new QueriesMysqlGetHandler())
            .addPrefixPath("/queries/postgresql", new QueriesPostgresqlGetHandler())
            .addPrefixPath("/updates/mysql", new UpdatesMysqlGetHandler())
            .addPrefixPath("/updates/postgresql", new UpdatesPostgresqlGetHandler())
        ;
    }
}

