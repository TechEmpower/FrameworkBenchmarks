package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path("/plaintext")
public class PlaintextResource {

  @GET
  @Produces("text/plain; charset=utf-8")
  public Object plaintext() {
    String data = "Hello, World!";
    return data;
  }
}
