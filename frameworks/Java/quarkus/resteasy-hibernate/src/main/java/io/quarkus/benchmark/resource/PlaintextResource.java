package io.quarkus.benchmark.resource;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/plaintext")
public class PlaintextResource {
    private static final String HELLO = "Hello, World!";

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String plaintext() {
        return HELLO;
    }
}
