package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import hello.domain.World;

import java.util.Map;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadLocalRandom;

import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;

import org.hibernate.IdentifierLoadAccess;
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
  public Object db(@QueryParam("queries") String queriesParam)
      throws ExecutionException, InterruptedException {

    final int queries = getQueries(queriesParam);
    final World[] worlds = new World[queries];
    final Random random = ThreadLocalRandom.current();
    final Session session = sessionFactory.openSession();
    session.setDefaultReadOnly(true);
    final IdentifierLoadAccess accessor = session.byId(World.class);

    Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
    for (int i = 0; i < queries; i++) {
      futureWorlds.put(i, Common.EXECUTOR.submit(
        new Callable<World>() {
          @Override
          public World call() throws Exception {
            return (World) accessor.load(random.nextInt(DB_ROWS) + 1);
          }
        }
      ));
    }

    for (int i = 0; i < queries; i++) {
      worlds[i] = futureWorlds.get(i).get();
    }

    return queries == 1 ? worlds[0] : worlds;
  }

  private int getQueries(String proto) {
    int result = 1;
    try {
      result = Integer.parseInt(proto);
    } catch (NumberFormatException e) {
      e.printStackTrace();
    }

    return Math.min(500, Math.max(1, result));
  }
}
