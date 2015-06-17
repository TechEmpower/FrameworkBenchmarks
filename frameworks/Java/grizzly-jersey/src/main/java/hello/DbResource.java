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
  public Object db(@QueryParam("queries") String queryParam, @QueryParam("single") boolean isSingle)
      throws ExecutionException, InterruptedException {

    final int queries = getQueries(queryParam);
    final World[] worlds = new World[queries];
    final Random random = ThreadLocalRandom.current();

    Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
    for (int i = 0; i < queries; i++) {
      futureWorlds.put(i, Common.EXECUTOR.submit(
        new Callable<World>() {
          @Override
          public World call() throws Exception {
            Session session = sessionFactory.openSession();
            session.setDefaultReadOnly(true);

            try {
              return (World) session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
            } finally {
              session.close();
            }
          }
        }
      ));
    }

    for (int i = 0; i < queries; i++) {
      worlds[i] = futureWorlds.get(i).get();
    }

    return isSingle ? worlds[0] : worlds;
  }

  private int getQueries(String proto) {
    int result = 1;
    try {
      if (proto != null && !proto.trim().isEmpty()) {
        result = Integer.parseInt(proto);
      }
    } catch (NumberFormatException e) {/* by test contract */}

    return Math.min(500, Math.max(1, result));
  }
}
