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
        return Handlers.routing()
            .add(Methods.GET, "/db/mysql", new DbMysqlGetHandler())
            .add(Methods.GET, "/db/postgresql", new DbPostgresqlGetHandler())
            .add(Methods.GET, "/fortunes/mysql", new FortunesMysqlGetHandler())
            .add(Methods.GET, "/fortunes/postgresql", new FortunesPostgresqlGetHandler())
            .add(Methods.GET, "/json", new JsonGetHandler())
            .add(Methods.GET, "/plaintext", new PlaintextGetHandler())
            .add(Methods.GET, "/queries/mysql", new QueriesMysqlGetHandler())
            .add(Methods.GET, "/queries/postgresql", new QueriesPostgresqlGetHandler())
            .add(Methods.GET, "/updates/mysql", new UpdatesMysqlGetHandler())
            .add(Methods.GET, "/updates/postgresql", new UpdatesPostgresqlGetHandler())
        ;
    }
}

