package io.quarkus.benchmark.resource;

import io.quarkus.benchmark.repository.FortuneRepository;
import io.quarkus.benchmark.rocker.VertxRawRockerOutputFactories;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerRequest;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import views.Fortunes;

@Singleton
public class FortunesHttpHandler {

    private static final CharSequence HTML_UTF8_CONTENT_TYPE = HttpHeaders.createOptimized("text/html; charset=UTF-8");

    @Inject
    VertxRawRockerOutputFactories factories;

    @Inject
    FortuneRepository fortuneRepository;

    public void handle(final HttpServerRequest request) {
        fortuneRepository.findAllSortedFortunes(fortunes -> {
            if (fortunes.succeeded()) {
                final var vertxRockerOutput = Fortunes.template(fortunes.result()).render(factories.ioFactory());
                var res = request.response();
                res.headers().add(HttpHeaders.CONTENT_TYPE, HTML_UTF8_CONTENT_TYPE);
                res.end(vertxRockerOutput.buffer(), null);
            } else {
                request.response().setStatusCode(500).end(fortunes.cause().getMessage());
            }
        });
    }
}
