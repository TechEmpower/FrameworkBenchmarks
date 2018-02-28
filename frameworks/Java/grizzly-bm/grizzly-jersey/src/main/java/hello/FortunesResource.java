package hello;

import static javax.ws.rs.core.MediaType.TEXT_HTML;

import com.sun.jersey.api.view.Viewable;
import com.sun.jersey.spi.resource.Singleton;
import hello.domain.Fortune;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

@Singleton
@Path("/fortunes")
public class FortunesResource {

  @Context
  private SessionFactory sessionFactory;

  @GET
  @Produces(TEXT_HTML + "; charset=utf-8")
  public Viewable fortunes() {
    Session session = sessionFactory.openSession();
    Criteria criteria = session.createCriteria(Fortune.class);
    @SuppressWarnings("unchecked")
    List<Fortune> fortunes = new ArrayList<>(criteria.list());
    session.close();
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(null);
    return new ViewableFortunes(fortunes);
  }

  private static final class ViewableFortunes extends Viewable {
    ViewableFortunes(List<Fortune> fortunes) {
      super("fortunes.mustache", fortunes);
    }

    @Override
    public boolean isTemplateNameAbsolute() {
      return true;
    }
  }
}
