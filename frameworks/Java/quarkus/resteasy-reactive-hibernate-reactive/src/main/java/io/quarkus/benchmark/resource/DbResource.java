package io.quarkus.benchmark.resource;

import java.util.List;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import io.quarkus.benchmark.utils.LocalRandom;
import io.quarkus.benchmark.utils.Randomizer;
import org.hibernate.FlushMode;
import org.hibernate.reactive.mutiny.Mutiny;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.WorldRepository;
import io.smallrye.context.api.CurrentThreadContext;
import io.smallrye.mutiny.Uni;
import org.eclipse.microprofile.context.ThreadContext;

@Produces(MediaType.APPLICATION_JSON)
@Path("/")
public class DbResource {

    @Inject
    WorldRepository worldRepository;

    @GET
    @Path("db")
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    public Uni<World> db() {
        return worldRepository.findStateless();
    }

    @GET
    @Path("queries")
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    public Uni<List<World>> queries(@QueryParam("queries") String queries) {
        final int queryCount = parseQueryCount(queries);
        return worldRepository.findStateless(queryCount);
    }

    @GET
    @Path("createData")
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    public Uni<Void> createData() {
        return worldRepository.createData();
    }

    private Uni<List<World>> randomWorldsForWrite(Mutiny.Session session, int count) {
        return worldRepository.findManaged(session, count);
    }

    @GET
    @Path("updates")
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    public Uni<List<World>> updates(@QueryParam("queries") String queries) {
        return worldRepository.inSession(session -> {

            session.setFlushMode(FlushMode.MANUAL);

            Uni<List<World>> worlds = randomWorldsForWrite(session, parseQueryCount(queries));
            return worlds.flatMap(worldsCollection -> {
                final LocalRandom localRandom = Randomizer.current();
                worldsCollection.forEach( w -> {
                    //Read the one field, as required by the following rule:
                    // # vi. At least the randomNumber field must be read from the database result set.
                    final int previousRead = w.getRandomNumber();
                    //Update it, but make sure to exclude the current number as Hibernate optimisations would have us "fail"
                    //the verification:
                    w.setRandomNumber(localRandom.getNextRandomExcluding(previousRead));
                } );

                return worldRepository.update(session, worldsCollection);
            });
        });
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