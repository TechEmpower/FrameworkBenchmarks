package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.util.concurrent.EventExecutor;
import io.quarkus.benchmark.filter.HttpResponseDecorator;
import io.quarkus.runtime.StartupEvent;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
import jakarta.annotation.PreDestroy;
import jakarta.enterprise.event.Observes;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import java.util.ArrayList;
import java.util.List;

@Singleton
public class HttpRoutes {
    private static final int PORT = 8080;
    private static final String PATH_PLAINTEXT = "/plaintext";
    private static final String PATH_JSON = "/json";
    private static final String PATH_DB = "/db";
    private static final String PATH_QUERIES = "/queries";
    private static final String PATH_UPDATES = "/updates";
    private static final String PATH_FORTUNES = "/fortunes";
    private HttpServer[] servers;
    @Inject
    private HttpResponseDecorator responseDecorator;
    @Inject
    private PlaintextHttpHandler plaintextHttpHandler;
    @Inject
    private JsonHttpHandler jsonHandler;
    @Inject
    private DbHttpHandler dbHandler;
    @Inject
    private UpdateHttpHandler updateHandler;
    @Inject
    private QueriesHttpHandler queriesHandler;
    @Inject
    private FortunesHttpHandler fortunesHandler;

    private static HttpServer[] createAndStartHttpServers(final Vertx vertx, final Handler<HttpServerRequest> requestHandler) {
        final var executors = new ArrayList<EventExecutor>(Runtime.getRuntime().availableProcessors());
        vertx.nettyEventLoopGroup().forEach(executors::add);
        final HttpServer[] servers = new HttpServer[executors.size()];
        final List<Future<?>> allHttpServerListening = new ArrayList<>(servers.length);
        for (int i = 0; i < servers.length; i++) {
            final int executorId = i;
            final var done = Promise.promise();
            allHttpServerListening.add(done.future());
            executors.get(executorId).execute(() -> {
                servers[executorId] = vertx.createHttpServer(new HttpServerOptions());
                servers[executorId]
                        .requestHandler(requestHandler)
                        .listen(PORT);
                done.complete();
            });
        }
        Future.join(allHttpServerListening).toCompletionStage().toCompletableFuture().join();
        return servers;
    }

    public void init(@Observes final StartupEvent startupEvent, final Vertx vertx) {
        servers = createAndStartHttpServers(vertx, this::handle);
    }

    @PreDestroy
    public void shutdown() {
        for (final HttpServer server : servers) {
            server.close();
        }
    }

    private void handle(final HttpServerRequest request) {
        try {
            responseDecorator.decorate(request.response());
            switch (request.path()) {
                case PATH_PLAINTEXT:
                    plaintextHttpHandler.handle(request);
                    break;
                case PATH_JSON:
                    jsonHandler.handle(request);
                    break;
                case PATH_DB:
                    dbHandler.handle(request);
                    break;
                case PATH_QUERIES:
                    queriesHandler.handle(request);
                    break;
                case PATH_UPDATES:
                    updateHandler.handle(request);
                    break;
                case PATH_FORTUNES:
                    fortunesHandler.handle(request);
                    break;
                default:
                    request.response().setStatusCode(HttpResponseStatus.NOT_FOUND.code()).end();
                    break;
            }
        } catch (final Exception e) {
            request.response().setStatusCode(HttpResponseStatus.INTERNAL_SERVER_ERROR.code()).end();
        }
    }
}
