
package com.example.helloworld.resources;

import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/json")
@Produces(MediaType.APPLICATION_JSON)
public class JsonResource
{
  // Response message class (copied from 'servlet' test)
  public final static class HelloMessage {
    public final String message;

    public HelloMessage(String m) { message = m; }
  }

  public JsonResource() { }

  @GET
  public HelloMessage sayHello()
  {
    return new HelloMessage("Hello, World!");
  }
}
