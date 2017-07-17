package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import com.sun.jersey.spi.resource.Singleton;
import hello.domain.World;
import java.util.concurrent.Callable;
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

@Singleton
@Path("/db")
public class DbResource {

  @Context
  private SessionFactory sessionFactory;

  @GET
  @Produces(APPLICATION_JSON)
  public Object db(@QueryParam("queries") String queryParam,
                   @QueryParam("single") boolean isSingle)
      throws ExecutionException, InterruptedException {

    int queries = getQueries(queryParam);

    @SuppressWarnings("unchecked")
    Future<World>[] futureWorlds = new Future[queries];
    for (int i = 0; i < queries; i++) {
      Callable<World> callable =
          () -> {
            int id = ThreadLocalRandom.current().nextInt(DB_ROWS) + 1;
            Session session = sessionFactory.openSession();
            session.setDefaultReadOnly(true);
            try {
              return (World) session.byId(World.class).load(id);
            } finally {
              session.close();
            }
          };
      futureWorlds[i] = Common.EXECUTOR.submit(callable);
    }

    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      worlds[i] = futureWorlds[i].get();
    }

    return isSingle ? worlds[0] : worlds;
  }

  private static int getQueries(String proto) {
    int result = 1;
    try {
      if (proto != null && !proto.trim().isEmpty()) {
        result = Integer.parseInt(proto);
      }
    } catch (NumberFormatException ignored) {/* by test contract */}

    return Math.min(500, Math.max(1, result));
  }

  private static final int DB_ROWS = 10000;
}
