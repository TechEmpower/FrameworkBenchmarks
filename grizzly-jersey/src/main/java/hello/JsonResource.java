package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path("/json")
public class JsonResource {
  
  @GET
  @Produces(APPLICATION_JSON + "; charset=utf-8")
  public Object json() {
    Map<String, String> data = new HashMap<String, String>(1);
    data.put("message", "Hello, World!");
    return data;
  }
}
