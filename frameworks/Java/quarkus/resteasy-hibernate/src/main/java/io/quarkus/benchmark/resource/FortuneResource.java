package io.quarkus.benchmark.resource;

import com.fizzed.rocker.Rocker;
import com.fizzed.rocker.RockerOutput;
import io.quarkus.benchmark.model.Fortune;
import io.quarkus.benchmark.repository.FortuneRepository;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Singleton
@Path("/")
@Produces(MediaType.TEXT_HTML + "; charset=UTF-8")
@Consumes(MediaType.APPLICATION_JSON)
public class FortuneResource {

    @Inject
    FortuneRepository repository;

    private static final String FORTUNES_MAP_KEY = "fortunes";
    private static final String FORTUNES_TEMPLATE_FILENAME = "Fortunes.rocker.html";
    private static final Comparator<Fortune> fortuneComparator = Comparator.comparing(fortune -> fortune.getMessage());

    @GET
    @Path("/fortunes")
    public String fortunes() {
        List<Fortune> fortunes = new ArrayList<>(repository.findAllStateless());
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        fortunes.sort(fortuneComparator);

        RockerOutput output = Rocker.template(FORTUNES_TEMPLATE_FILENAME)
                .bind(Collections.singletonMap(FORTUNES_MAP_KEY, fortunes))
                .render();

        return output.toString();
    }
}
