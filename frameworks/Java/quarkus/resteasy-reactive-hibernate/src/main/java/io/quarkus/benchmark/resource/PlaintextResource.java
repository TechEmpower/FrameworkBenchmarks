package io.quarkus.benchmark.resource;

import io.smallrye.common.annotation.NonBlocking;
import io.vertx.core.buffer.Buffer;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import java.nio.charset.StandardCharsets;

@Path("/plaintext")
public class PlaintextResource {

    // We prefer an heap Buffer, because Resteasy-Reactive would perform Buffer::getBytes on it
    private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer("Hello, world!".getBytes(StandardCharsets.UTF_8));

    @Produces(MediaType.TEXT_PLAIN)
    @GET
    @NonBlocking
    public Buffer plaintext() {
        return HELLO_WORLD_BUFFER;
    }
}
