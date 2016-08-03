package hello;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path("/plaintext")
public class PlaintextResource {

  @GET
  @Produces("text/plain")
  public Object plaintext() {
    String data = "Hello, World!";
    return data;
  }
}
