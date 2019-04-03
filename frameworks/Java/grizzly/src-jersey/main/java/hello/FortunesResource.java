package hello;

import static javax.ws.rs.core.MediaType.TEXT_HTML;
import hello.domain.Fortune;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import org.glassfish.jersey.server.mvc.Template;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

@Singleton
@Path("/fortunes")
public class FortunesResource {
	@Inject
	private SessionFactory sessionFactory;

	@SuppressWarnings("unchecked")
	@GET
	@Produces(TEXT_HTML + "; charset=utf-8")
	@Template(name = "/fortunes.mustache")
	public List<Fortune> fortunes() {
		List<Fortune> fortunes = null;
		Session session = sessionFactory.openSession();
		Criteria criteria = session.createCriteria(Fortune.class);
		fortunes = new ArrayList<>(criteria.list());
		session.close();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(null);
		return fortunes;
	}
}