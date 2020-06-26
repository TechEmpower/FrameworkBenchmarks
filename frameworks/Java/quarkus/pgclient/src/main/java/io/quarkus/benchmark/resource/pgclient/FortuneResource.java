package io.quarkus.benchmark.resource.pgclient;

import java.io.StringWriter;
import java.util.Collections;
import java.util.Comparator;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.pgclient.FortuneRepository;
import io.quarkus.vertx.web.Route;
import io.vertx.ext.web.RoutingContext;

@ApplicationScoped
public class FortuneResource extends BaseResource {

    @Inject
    FortuneRepository repository;

    private final Mustache template;

    public FortuneResource() {
        MustacheFactory mf = new DefaultMustacheFactory();
        template = mf.compile("fortunes.mustache");
    }

    @Route(path = "fortunes")
    public void fortunes(RoutingContext rc) {
        repository.findAll()
        .subscribe().with( fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(Comparator.comparing(fortune -> fortune.getMessage()));
            StringWriter writer = new StringWriter();
            template.execute(writer, Collections.singletonMap("fortunes", fortunes));
            rc.response().putHeader("Content-Type", "text/html;charset=UTF-8");
            rc.response().end(writer.toString());
        },
                           t -> handleFail(rc, t));
    }
}
