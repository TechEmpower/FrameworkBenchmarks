package com.techempower.ee7.tests;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.techempower.ee7.util.Helpers;

@Path("/plaintext")
public class PlainText {
  private static final String MESSAGE = "Hello, World!";

  @GET
  @Produces(MediaType.TEXT_PLAIN)
  public Response get() {
    return Helpers.planTextResponse(MESSAGE);
  }
}
