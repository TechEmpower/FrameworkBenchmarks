package com.example.helloworld.resources;

import com.codahale.metrics.annotation.Timed;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/plaintext")
@Produces(MediaType.TEXT_PLAIN)
public class TextResource {

    @GET
    @Timed
    public String sayHello() {
        return "Hello, World!";
    }
}
