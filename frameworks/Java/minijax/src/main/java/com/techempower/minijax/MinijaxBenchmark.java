package com.techempower.minijax;

import static java.util.Collections.*;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.MediaType;

import org.minijax.Minijax;
import org.minijax.db.PersistenceFeature;
import org.minijax.json.JsonFeature;
import org.minijax.mustache.MustacheFeature;
import org.minijax.view.View;

public class MinijaxBenchmark {

    @Inject
    private Dao dao;

    @GET
    @Path("/plaintext")
    @Produces(MediaType.TEXT_PLAIN)
    public static String plaintext() {
        return "Hello, World!";
    }

    @GET
    @Path("/json")
    @Produces(MediaType.APPLICATION_JSON)
    public static Map<String, String> json() {
        return singletonMap("message", "Hello, World!");
    }

    @GET
    @Path("/db")
    @Produces(MediaType.APPLICATION_JSON)
    public World db() {
        return dao.getRandomWorld();
    }

    @GET
    @Path("/queries")
    @Produces(MediaType.APPLICATION_JSON)
    public List<World> queries(@QueryParam("queries") final String count) {
        return dao.getWorlds(parseCount(count));
    }

    @GET
    @Path("/fortunes")
    @Produces("text/html;charset=UTF-8")
    public View fortunes() {
        final List<Fortune> fortunes = dao.getAllFortunes();
        fortunes.add(new Fortune("Additional fortune added at request time."));
        Collections.sort(fortunes);

        final View view = new View("fortunes");
        view.getModel().put("fortunes", fortunes);
        return view;
    }

    @GET
    @Path("/updates")
    @Produces(MediaType.APPLICATION_JSON)
    public List<World> updates(@QueryParam("queries") final String count) {
        final List<World> worlds = dao.getWorlds(parseCount(count));
        for (final World world : worlds) {
            world.setRandomNumber(Dao.randomWorld());
        }
        dao.batchUpdate(worlds);
        return worlds;
    }

    private static class ServerHeaderFilter implements ContainerResponseFilter {
        @Override
        public void filter(final ContainerRequestContext request, final ContainerResponseContext response) {
            response.getHeaders().add("Server", "M");
        }
    }

    private static int parseCount(final String count) {
        if (count == null || count.isEmpty()) {
            return 1;
        }
        try {
            return Integer.parseInt(count);
        } catch (final NumberFormatException ex) {
            return 1;
        }
    }

    public static void main(final String[] args) throws IOException {
        new Minijax()
                .properties("minijax.properties")
                .register(PersistenceFeature.class)
                .register(JsonFeature.class)
                .register(MustacheFeature.class)
                .register(MinijaxBenchmark.class)
                .register(ServerHeaderFilter.class)
                .start();
    }
}
