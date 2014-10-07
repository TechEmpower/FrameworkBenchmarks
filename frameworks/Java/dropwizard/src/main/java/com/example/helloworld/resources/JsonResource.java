package com.example.helloworld.resources;

import com.example.helloworld.resources.api.HelloMessage;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/json")
@Produces(MediaType.APPLICATION_JSON)
public class JsonResource {

    @GET
    public HelloMessage sayHello() {
        return new HelloMessage("Hello, World!");
    }
}
