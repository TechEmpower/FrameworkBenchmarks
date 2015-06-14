package com.example.helloworld.resources;

import com.example.helloworld.db.model.World;
import com.google.common.base.Optional;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.DBUpdate;
import org.mongojack.JacksonDBCollection;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import static com.example.helloworld.resources.Helper.getQueries;

@Path("/mongo/db")
@Produces(MediaType.APPLICATION_JSON)
public class MongoWorldResource {
    private final JacksonDBCollection<World, Long> worldsCollection;

    public MongoWorldResource(JacksonDBCollection<World, Long> worldsCollection) {
        this.worldsCollection = worldsCollection;
    }

    @GET
    public Object dbTest(@QueryParam("queries") Optional<String> queries) {
        int totalQueries = getQueries(queries);
        final World[] worlds = new World[totalQueries];

        for (int i = 0; i < totalQueries; i++) {
            worlds[i] = worldsCollection.findOneById(
                    (long) Helper.randomWorld(),
                    DBProjection.include("_id", "randomNumber")
            );
        }
        if (!queries.isPresent()) {
            return worlds[0];
        } else {
            return worlds;
        }
    }

    @GET
    @Path("/update")
    public World[] updateTest(@QueryParam("queries") Optional<String> queries) {
        int totalQueries = getQueries(queries);
        final World[] worlds = new World[totalQueries];

        for (int i = 0; i < totalQueries; i++) {
            // use findAndModify() similar to nodejs-mongodb-raw
            worlds[i] = worldsCollection.findAndModify(
                    DBQuery.is("_id", (long) Helper.randomWorld()),
                    DBProjection.include("_id", "randomNumber"),
                    null,
                    false,
                    DBUpdate.set("randomNumber", (long) Helper.randomWorld()),
                    true,
                    false
            );
        }
        return worlds;
    }
}
