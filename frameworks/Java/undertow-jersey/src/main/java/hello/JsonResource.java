package hello;

import javax.inject.*;
import javax.ws.rs.*;
import java.util.*;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

@Singleton
@Path("/json")
public class JsonResource
{

  @GET
  @Produces(APPLICATION_JSON)
  public Object json()
  {
    Map<String, String> data = new HashMap<String, String>(1);
    data.put("message", "Hello, World!");
    return data;
  }
}
