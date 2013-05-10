package hello;

import static javax.ws.rs.core.MediaType.TEXT_HTML;
import hello.domain.Fortune;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;

import org.hibernate.Session;
import org.hibernate.SessionFactory;

import com.sun.jersey.api.view.Viewable;
import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path("/fortunes")
public class FortunesResource {

  @Context
  private SessionFactory sessionFactory;
  
  @GET
  @Produces(TEXT_HTML + "; charset=utf-8")
  public Viewable fortunes() {
    final Session session = sessionFactory.openSession();
    final List fortunes = new ArrayList(session.createCriteria(Fortune.class).list());
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);
    
    session.close();
    return new Viewable("/fortunes.mustache", new Scope(fortunes));
  }
  
  public static class Scope {
    public List fortunes;
    
    public Scope(final List fortunes) {
      this.fortunes = fortunes;
    }
  }
}
