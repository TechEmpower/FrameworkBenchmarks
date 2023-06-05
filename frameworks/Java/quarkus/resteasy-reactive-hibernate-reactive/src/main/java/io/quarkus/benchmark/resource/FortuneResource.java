package io.quarkus.benchmark.resource;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.smallrye.mutiny.Uni;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.templ.rocker.impl.VertxBufferOutput;

import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import java.util.Comparator;

@Path("/fortunes")
public class FortuneResource  {

    @Inject
    FortuneRepository repository;

    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());

    @Produces("text/html; charset=UTF-8")
    @GET
    public Uni<Buffer> fortunes() {
        return repository.findAll()
                .map(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(fortuneComparator);
                    return views.Fortunes.template(fortunes)
                            .render(VertxBufferOutput.FACTORY)
                            .getBuffer();
                });
    }
}
