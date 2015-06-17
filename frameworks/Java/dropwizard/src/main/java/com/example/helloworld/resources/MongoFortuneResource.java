package com.example.helloworld.resources;

import com.example.helloworld.db.model.Fortune;
import com.example.helloworld.resources.views.FortuneView;
import com.google.common.collect.Lists;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.JacksonDBCollection;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.Collections;
import java.util.List;

@Path("/mongo/fortunes")
@Produces(MediaType.TEXT_HTML + ";charset=UTF-8")
public class MongoFortuneResource {

    private final JacksonDBCollection<Fortune, Long> fortuneCollection;

    public MongoFortuneResource(JacksonDBCollection<Fortune, Long> fortuneCollection) {
        this.fortuneCollection = fortuneCollection;
    }

    @GET
    public FortuneView dbTest() {
        final List<Fortune> fortunes = Lists.newArrayListWithExpectedSize(32);

        fortunes.addAll(fortuneCollection.find(DBQuery.empty(), DBProjection.include("_id", "message")).toArray());
        fortunes.add(new Fortune("Additional fortune added at request time."));

        Collections.sort(fortunes);
        return new FortuneView(fortunes);
    }
}
