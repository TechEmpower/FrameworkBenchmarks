package hello;

import javax.inject.*;
import javax.ws.rs.*;
import java.util.*;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

@Singleton
@Path("/plaintext")
public class PlaintextResource
{
  @GET
  @Produces("text/plain")
  public String plaintext()
  {
    return "Hello, World!";
  }
}
