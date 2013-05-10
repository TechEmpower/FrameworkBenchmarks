package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import hello.domain.World;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;

import org.hibernate.Session;
import org.hibernate.SessionFactory;

import com.sun.jersey.spi.resource.Singleton;

@Singleton
@Path("/db")
public class DbResource {

  private static final int DB_ROWS = 10000;
  
  @Context
  private SessionFactory sessionFactory;
  
  @GET
  @Produces(APPLICATION_JSON + "; charset=utf-8")
  public Object db(@QueryParam("queries") @DefaultValue("1") final int queries) {
    final World[] worlds = new World[queries];
    final Random random = ThreadLocalRandom.current();
    final Session session = sessionFactory.openSession();
    
    for (int i = 0; i < queries; i++) {
        worlds[i] = (World) session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
    }
    
    session.close();
    return worlds;
  }
  
}
