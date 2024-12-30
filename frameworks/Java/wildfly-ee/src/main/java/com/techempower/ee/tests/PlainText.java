package com.techempower.ee7.tests;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/plaintext")
public class PlainText {
    private static final String MESSAGE = "Hello, World!";

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String get() {
        return MESSAGE;
    }

    @GET
    @Path("hi")
    public String get2() {
        return "Hello WildFly!";
    }

    @GET
    @Path("/bye")
    public String get3() {
        return "Bye WildFly!";
    }

}
