package io.quarkus.benchmark.resource;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

import io.smallrye.common.annotation.NonBlocking;

@Path("/json")
public class JsonResource  {

    private static final String HELLO = "Hello, World!";

    @Produces(MediaType.APPLICATION_JSON)
    @GET
    @NonBlocking
    public Message json() {
        // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
        return new Message(HELLO);
    }
}

