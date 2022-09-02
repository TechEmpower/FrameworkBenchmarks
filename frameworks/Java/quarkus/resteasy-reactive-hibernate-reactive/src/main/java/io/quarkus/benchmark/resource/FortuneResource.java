package io.quarkus.benchmark.resource;

import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.smallrye.context.api.CurrentThreadContext;
import io.smallrye.mutiny.Uni;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.templ.rocker.impl.VertxBufferOutput;
import org.eclipse.microprofile.context.ThreadContext;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import java.util.Comparator;

@Path("/fortunes")
public class FortuneResource  {

    @Inject
    FortuneRepository repository;

    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());

    @Produces("text/html; charset=UTF-8")
    @GET
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
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
