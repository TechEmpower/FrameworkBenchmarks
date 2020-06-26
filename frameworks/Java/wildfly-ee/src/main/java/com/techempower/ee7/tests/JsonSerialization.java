package com.techempower.ee7.tests;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/json")
public class JsonSerialization {

    private static final String MESSAGE = "Hello, World!";

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public JsonResponse get() {
        return new JsonResponse(MESSAGE);
    }

    public class JsonResponse {

        private final String message;

        public JsonResponse(String message) {
            this.message = message;
        }

        public String getMessage() {
            return message;
        }
    }
}
