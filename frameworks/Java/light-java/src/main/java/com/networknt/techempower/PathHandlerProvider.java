package com.networknt.techempower;

import com.networknt.handler.HandlerProvider;
import com.networknt.techempower.handler.*;
import io.undertow.Handlers;
import io.undertow.server.HttpHandler;
import io.undertow.server.handlers.BlockingHandler;

public class PathHandlerProvider implements HandlerProvider {
    @Override
    public HttpHandler getHandler() {
        return Handlers.path()
            .addExactPath("/plaintext", new PlaintextGetHandler())
            .addExactPath("/json", new JsonGetHandler())
            .addExactPath("/db", new BlockingHandler(new DbPostgresqlGetHandler()))
            .addExactPath("/fortunes", new BlockingHandler(new FortunesPostgresqlGetHandler()))
            .addExactPath("/queries", new BlockingHandler(new QueriesPostgresqlGetHandler()))
            .addExactPath("/updates", new BlockingHandler(new UpdatesPostgresqlGetHandler()))
        ;
    }

    public static PathHandlerProvider create() {
        return new PathHandlerProvider();
    }
}

