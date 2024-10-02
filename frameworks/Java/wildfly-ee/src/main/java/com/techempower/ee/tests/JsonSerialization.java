package com.techempower.ee7.tests;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

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
