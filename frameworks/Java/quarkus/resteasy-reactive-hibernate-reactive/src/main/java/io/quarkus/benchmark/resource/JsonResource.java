package io.quarkus.benchmark.resource;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import io.smallrye.common.annotation.NonBlocking;
import io.smallrye.context.api.CurrentThreadContext;
import org.eclipse.microprofile.context.ThreadContext;

@Path("/json")
public class JsonResource  {

    private static final String HELLO = "Hello, World!";

    @Produces(MediaType.APPLICATION_JSON)
    @GET
    @CurrentThreadContext(propagated = {}, cleared = {}, unchanged = ThreadContext.ALL_REMAINING)
    @NonBlocking
    public Message json() {
        // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
        return new Message(HELLO);
    }
}

