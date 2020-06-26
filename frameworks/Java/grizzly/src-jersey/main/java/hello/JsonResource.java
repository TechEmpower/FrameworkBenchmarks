package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import java.util.Collections;

import javax.inject.Singleton;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

@Singleton
@Path("/json")
public class JsonResource {

	@GET
	@Produces(APPLICATION_JSON)
	public Object json() {
		return Collections.singletonMap("message", "Hello, World!");
	}
}
