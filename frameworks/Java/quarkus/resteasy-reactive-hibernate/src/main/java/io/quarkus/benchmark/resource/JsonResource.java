package io.quarkus.benchmark.resource;

import io.smallrye.common.annotation.NonBlocking;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/json")
public class JsonResource {
    private static final String HELLO = "Hello, World!";

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @NonBlocking
    public Message json() {
        return new Message(HELLO);
    }
}

