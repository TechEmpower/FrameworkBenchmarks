package com.techempower.ee7.tests;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.techempower.ee7.util.Helpers;

@Path("/json")
public class JsonSerialization {

  private static final String MESSAGE = "Hello, World!";

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  public Response get() {
    return Helpers.jsonResponse(new JsonResponse(MESSAGE));
  }

  @XmlRootElement
  public class JsonResponse {

    private final String message;

    public JsonResponse(String message) {
      this.message = message;
    }

    @XmlElement
    public String getMessage() {
      return message;
    }
  }
}
