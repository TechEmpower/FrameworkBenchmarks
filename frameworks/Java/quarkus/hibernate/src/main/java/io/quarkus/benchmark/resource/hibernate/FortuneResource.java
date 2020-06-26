package io.quarkus.benchmark.resource.hibernate;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

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
import io.quarkus.benchmark.model.hibernate.Fortune;
import io.quarkus.benchmark.repository.hibernate.FortuneRepository;

@ApplicationScoped
@Path("/")
@Produces(MediaType.TEXT_HTML)
@Consumes(MediaType.APPLICATION_JSON)
public class FortuneResource {

    @Inject
    FortuneRepository repository;

    private final Mustache template;
    private final Comparator<Fortune> fortuneComparator;

    public FortuneResource() {
        MustacheFactory mf = new DefaultMustacheFactory();
        template = mf.compile("fortunes.mustache");
        fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());
    }

    @GET
    @Path("/fortunes")
    public String fortunes() {
        List<Fortune> fortunes = new ArrayList<>(repository.findAllStateless());
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        fortunes.sort(fortuneComparator);

        StringWriter writer = new StringWriter();
        template.execute(writer, Collections.singletonMap("fortunes", fortunes));

        return writer.toString();
    }
}
