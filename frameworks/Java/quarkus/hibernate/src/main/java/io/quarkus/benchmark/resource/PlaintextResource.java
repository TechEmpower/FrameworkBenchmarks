package io.quarkus.benchmark.resource;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/plaintext")
public class PlaintextResource {
    private static final String HELLO = "Hello, World!";

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String plaintext() {
        return HELLO;
    }
}
