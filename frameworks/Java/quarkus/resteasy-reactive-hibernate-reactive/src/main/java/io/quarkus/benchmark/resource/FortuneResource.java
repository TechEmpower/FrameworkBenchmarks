package io.quarkus.benchmark.resource;

import com.fizzed.rocker.RenderingException;
import com.fizzed.rocker.runtime.DefaultHtmlStringify;
import com.fizzed.rocker.runtime.DefaultRockerTemplate;
import com.fizzed.rocker.runtime.GuavaHtmlStringify;
import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;
import io.quarkus.benchmark.utils.rocker.HtmlUtf8BufferRockerOutput;
import io.smallrye.mutiny.Uni;
import io.vertx.core.buffer.Buffer;

import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import views.Fortunes;

import java.util.Comparator;

@Path("/fortunes")
public class FortuneResource  {

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

    @Produces("text/html; charset=UTF-8")
    @GET
    public Uni<Buffer> fortunes() {
        return repository.findAll()
                .map(fortunes -> {
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
                });
    }
}
