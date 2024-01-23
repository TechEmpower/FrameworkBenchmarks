package io.quarkus.benchmark.resource;

import com.fizzed.rocker.RenderingException;
import com.fizzed.rocker.runtime.DefaultHtmlStringify;
import com.fizzed.rocker.runtime.DefaultRockerTemplate;
import com.fizzed.rocker.runtime.GuavaHtmlStringify;
import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.quarkus.benchmark.utils.rocker.HtmlUtf8BufferRockerOutput;
import io.vertx.core.buffer.Buffer;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import views.Fortunes;

import java.util.Comparator;
import java.util.List;

@Singleton
@Path("/")
@Produces(MediaType.TEXT_HTML+"; charset=UTF-8")
@Consumes(MediaType.APPLICATION_JSON)
public class FortuneResource {

    private static final DefaultHtmlStringify STRINGIFY = new GuavaHtmlStringify();

    private static class CustomFortunesTemplate extends Fortunes.Template {

        public CustomFortunesTemplate(Fortunes model) {
            super(model);
            __internal.setStringify(STRINGIFY);
        }
    }

    @Inject
    FortuneRepository repository;

    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(Fortune::getMessage);

    @GET
    @Path("/fortunes")
    public Buffer fortunes() {
        List<Fortune> fortunes = repository.findAllStateless();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        fortunes.sort(fortuneComparator);
        return new Fortunes() {

            @Override
            protected DefaultRockerTemplate buildTemplate() throws RenderingException {
                return new CustomFortunesTemplate(this);
            }
        }.fortunes(fortunes)
                .render(HtmlUtf8BufferRockerOutput.threadLocalFactory())
                .buffer();
    }

}
