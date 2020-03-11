package io.quarkus.benchmark.resource.pgclient;

import java.io.StringWriter;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.pgclient.FortuneRepository;

@ApplicationScoped
@Path("/pgclient")
@Produces(MediaType.TEXT_HTML)
@Consumes(MediaType.APPLICATION_JSON)
public class FortuneResource {

    @Inject
    FortuneRepository repository;

    private final Mustache template;

    public FortuneResource() {
        MustacheFactory mf = new DefaultMustacheFactory();
        template = mf.compile("fortunes.mustache");
    }

    @GET
    @Path("/fortunes")
    public CompletionStage<String> fortunes() {
        return repository.findAll()
                .map(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(Comparator.comparing(fortune -> fortune.getMessage()));

                    StringWriter writer = new StringWriter();
                    template.execute(writer, Collections.singletonMap("fortunes", fortunes));

                    return writer.toString();
                }).to(m -> {
                    CompletableFuture<String> cf = new CompletableFuture<>();
                    m.subscribe(cf::complete, cf::completeExceptionally);
                    return cf;
                });
    }
}
