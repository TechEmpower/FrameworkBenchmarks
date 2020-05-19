package hello;

import static javax.ws.rs.core.MediaType.TEXT_HTML;
import hello.domain.Fortune;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.persistence.EntityManagerFactory;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import org.glassfish.jersey.server.mvc.Template;

@Singleton
@Path("/fortunes")
public class FortunesResource {
	@Inject
	private EntityManagerFactory emf;

	@GET
	@Produces(TEXT_HTML + "; charset=utf-8")
	@Template(name = "/fortunes.mustache")
	public List<Fortune> fortunes() {
		List<Fortune> fortunes = new ArrayList<>(emf.createEntityManager()
				.createQuery("SELECT f FROM Fortune f", Fortune.class).getResultList());
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(null);
		return fortunes;
	}
}