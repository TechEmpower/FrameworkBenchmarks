package io.quarkus.benchmark.resource;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.templ.rocker.impl.VertxBufferOutput;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import java.util.Comparator;
import java.util.List;

@Singleton
@Path("/")
@Produces(MediaType.TEXT_HTML+"; charset=UTF-8")
@Consumes(MediaType.APPLICATION_JSON)
public class FortuneResource {

    @Inject
    FortuneRepository repository;

    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());

    @GET
    @Path("/fortunes")
    public Buffer fortunes() {
        List<Fortune> fortunes = repository.findAllStateless();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        fortunes.sort(fortuneComparator);
        return views.Fortunes.template(fortunes)
                .render(VertxBufferOutput.FACTORY)
                .getBuffer();
    }

}
