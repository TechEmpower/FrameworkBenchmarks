
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
  private final Map<String, String> MESSAGE = new HashMap<String, String>();

  public JsonResource()
  {
    MESSAGE.put("message", "Hello, world!");
  }

  @GET
  public Map<String, String> sayHello()
  {
    return MESSAGE;
  }
}
