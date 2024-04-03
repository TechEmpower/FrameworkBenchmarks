package io.quarkus.benchmark.resource;

import java.util.Collections;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.rocker.VertxRawRockerOutputFactories;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.quarkus.vertx.web.Route;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import views.Fortunes;

@Singleton
public class FortuneResource extends BaseResource {

    private static final CharSequence HTML_UTF8_CONTENT_TYPE = HttpHeaders.createOptimized("text/html; charset=UTF-8");

    @Inject
    FortuneRepository repository;
    @Inject
    VertxRawRockerOutputFactories factories;

    public FortuneResource() {

    }

    @Route(path = "fortunes")
    public void fortunes(final RoutingContext rc) {
        repository.findAll()
                .subscribe()
                .with(fortunes -> {
                            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                            Collections.sort(fortunes);
                            final var vertxRockerOutput = Fortunes.template(fortunes).render(factories.ioFactory());
                            var res = rc.response();
                            res.headers().add(HttpHeaders.CONTENT_TYPE, HTML_UTF8_CONTENT_TYPE);
                            res.end(vertxRockerOutput.buffer(), null);
                        },
                        t -> handleFail(rc, t));
    }
}