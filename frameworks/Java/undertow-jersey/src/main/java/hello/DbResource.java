package hello;

import hello.domain.*;
import org.hibernate.*;

import javax.inject.*;
import javax.ws.rs.*;
import java.util.*;
import java.util.concurrent.*;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

@Singleton
@Path("/db")
public class DbResource
{

  private static final int DB_ROWS = 10000;

  @Inject
  private SessionFactory sessionFactory;

  @GET
  @Produces(APPLICATION_JSON)
  public Object db(@QueryParam("queries") String queryParam,
      @QueryParam("single") boolean isSingle)
      throws ExecutionException, InterruptedException
  {

    final int queries = getQueries(queryParam);
    final World[] worlds = new World[queries];
    final Random random = ThreadLocalRandom.current();

    Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
    for (int i = 0; i < queries; i++)
    {
      futureWorlds.put(i, Common.EXECUTOR.submit(new Callable<World>()
      {
        @Override
        public World call() throws Exception
        {
          Session session = sessionFactory.openSession();
          session.setDefaultReadOnly(true);

          try
          {
            return (World)session.byId(World.class).load(
                random.nextInt(DB_ROWS) + 1);
          }
          finally
          {
            session.close();
          }
        }
      }));
    }

    for (int i = 0; i < queries; i++)
    {
      worlds[i] = futureWorlds.get(i).get();
    }
    return isSingle ? worlds[0] : worlds;
  }

  private int getQueries(String proto)
  {
    int result = 1;
    try
    {
      if (proto != null && !proto.trim().isEmpty())
      {
        result = Integer.parseInt(proto);
      }
    }
    catch (NumberFormatException e)
    {/* by test contract */}

    return Math.min(500, Math.max(1, result));
  }
}
