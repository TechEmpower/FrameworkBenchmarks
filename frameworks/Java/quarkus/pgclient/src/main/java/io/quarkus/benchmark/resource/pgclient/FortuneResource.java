package io.quarkus.benchmark.resource.pgclient;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.pgclient.FortuneRepository;
import io.quarkus.vertx.web.Route;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;
import io.vertx.reactivex.ext.web.templ.rocker.RockerTemplateEngine;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.util.Comparator;

@ApplicationScoped
public class FortuneResource extends BaseResource {

    @Inject
    FortuneRepository repository;

    private final RockerTemplateEngine templeEngine;

    public FortuneResource() {
        templeEngine = RockerTemplateEngine.create();
    }

    @Route(path = "fortunes")
    public void fortunes(RoutingContext rc) {
        repository.findAll()
                .subscribe().with(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(Comparator.comparing(fortune -> fortune.getMessage()));
                    rc.put("fortunes", fortunes);
                    templeEngine.render(rc.data(), "templates/Fortunes.rocker.html", res -> {
                        if (res.succeeded()) {
                            rc.response()
                                    .putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
                                    .end(res.result().toString());
                        } else {
                            rc.fail(res.cause());
                        }
                    });
                },
                t -> handleFail(rc, t));
    }
}
