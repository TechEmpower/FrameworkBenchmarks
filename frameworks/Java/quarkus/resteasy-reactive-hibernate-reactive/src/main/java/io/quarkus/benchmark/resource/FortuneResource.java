package io.quarkus.benchmark.resource;

import com.fizzed.rocker.Rocker;
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
import java.util.Collections;
import java.util.Comparator;

@Path("/fortunes")
public class FortuneResource  {

    @Inject
    FortuneRepository repository;

    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());

    private static final String FORTUNES_MAP_KEY = "fortunes";
    private static final String FORTUNES_TEMPLATE_FILENAME = "Fortunes.rocker.html";

    @Produces("text/html; charset=UTF-8")
    @GET
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    public Uni<Buffer> fortunes() {
        return repository.findAll()
                .map(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(fortuneComparator);
                    return Rocker.template(FORTUNES_TEMPLATE_FILENAME)
                            .bind(Collections.singletonMap(FORTUNES_MAP_KEY, fortunes))
                            .render(VertxBufferOutput.FACTORY)
                            .getBuffer();
                });
    }
}
