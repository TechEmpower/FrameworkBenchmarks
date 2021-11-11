package hello;

import javax.inject.Singleton;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

@Singleton
@Path("/plaintext")
public class PlaintextResource {

	@GET
	@Produces("text/plain")
	public Object plaintext() {
		return "Hello, World!";
	}
}