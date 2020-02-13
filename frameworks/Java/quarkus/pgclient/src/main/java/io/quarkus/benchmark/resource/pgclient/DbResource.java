package io.quarkus.benchmark.resource.pgclient;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ThreadLocalRandom;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.pgclient.WorldRepository;
import io.reactivex.Maybe;
import io.reactivex.Single;

@ApplicationScoped
@Path("/pgclient")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DbResource {

    private static Logger LOG = LoggerFactory.getLogger(WorldRepository.class);

    @Inject
    WorldRepository worldRepository;

    @GET
    @Path("/db")
    public CompletionStage<World> db() {
        return randomWorld()
                .to(m -> {
                    CompletableFuture<World> cf = new CompletableFuture<>();
                    m.subscribe(cf::complete, cf::completeExceptionally, () -> cf.complete(null));
                    return cf;
                });
    }

    @GET
    @Path("/queries")
    public CompletionStage<List<World>> queries(@QueryParam("queries") String queries) {
        Maybe<World>[] worlds = new Maybe[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> randomWorld());

        return Maybe.concatArray(worlds)
                .toList()
                .to(m -> {
                    CompletableFuture<List<World>> cf = new CompletableFuture<>();
                    m.subscribe(cf::complete, cf::completeExceptionally);
                    return cf;
                });
    }

    @GET
    @Path("/updates")
    public CompletionStage<List<World>> updates(@QueryParam("queries") String queries) {
        Single<World>[] worlds = new Single[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> randomWorld().flatMapSingle(world -> {
            world.setId(randomWorldNumber());
            return worldRepository.update(world);
        }));

        return Single.concatArray(worlds)
                .toList()
                .to(m -> {
                    CompletableFuture<List<World>> cf = new CompletableFuture<>();
                    m.subscribe(cf::complete, cf::completeExceptionally);
                    return cf;
                });
    }

    private Maybe<World> randomWorld() {
        return worldRepository.find(randomWorldNumber());
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
