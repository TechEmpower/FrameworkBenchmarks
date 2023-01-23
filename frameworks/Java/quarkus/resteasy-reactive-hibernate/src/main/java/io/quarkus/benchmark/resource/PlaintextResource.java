package io.quarkus.benchmark.resource;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.ByteBufAllocator;
import io.smallrye.common.annotation.NonBlocking;
import io.vertx.core.buffer.Buffer;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.nio.charset.StandardCharsets;

@Path("/plaintext")
public class PlaintextResource {
    private static final String HELLO_WORLD = "Hello, world!";
    private static final Buffer HELLO_WORLD_BUFFER;

    static {
        ByteBuf nettyBuffer = ByteBufAllocator.DEFAULT.directBuffer();
        nettyBuffer.writeBytes(HELLO_WORLD.getBytes(StandardCharsets.UTF_8));
        HELLO_WORLD_BUFFER = Buffer.buffer(nettyBuffer);
    }

    @Produces(MediaType.TEXT_PLAIN)
    @GET
    @NonBlocking
    public Buffer plaintext() {
        return HELLO_WORLD_BUFFER;
    }
}
