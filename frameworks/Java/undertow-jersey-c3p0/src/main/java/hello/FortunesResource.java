package hello;

import hello.domain.*;
import org.glassfish.jersey.server.mvc.*;
import org.hibernate.*;

import javax.inject.*;
import javax.ws.rs.*;
import java.util.*;

import static javax.ws.rs.core.MediaType.TEXT_HTML;

@Singleton
@Path("/fortunes")
public class FortunesResource
{

  @Inject
  private SessionFactory sessionFactory;

  @GET
  @Produces(TEXT_HTML + "; charset=utf-8")
  public Viewable fortunes()
  {
    final Session session = sessionFactory.openSession();
    final List<Fortune> fortunes = new ArrayList<>(
        session.createCriteria(Fortune.class).list());
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);

    session.close();
    return new Viewable("/fortunes.mustache", new Scope(fortunes));
  }

  public static class Scope
  {
    public List fortunes;

    public Scope(final List fortunes)
    {
      this.fortunes = fortunes;
    }
  }
}
