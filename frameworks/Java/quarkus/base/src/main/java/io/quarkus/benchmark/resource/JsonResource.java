package io.quarkus.benchmark.resource;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/json")
public class JsonResource {
    private static final String MESSAGE = "message";
    private static final String HELLO = "Hello, World!";

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public CompletionStage<Map<String, String>> json() {
        return CompletableFuture.supplyAsync(() -> Map.of(MESSAGE, HELLO));
    }
}
