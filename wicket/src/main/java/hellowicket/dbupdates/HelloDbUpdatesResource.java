package hellowicket.dbupdates;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.wicket.request.resource.AbstractResource;
import org.hibernate.CacheMode;
import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;

import com.fasterxml.jackson.databind.ObjectMapper;

import hellowicket.HibernateUtil;
import hellowicket.World;
import org.hibernate.Transaction;

/**
 * A resource that implements the requirements for
 * <a href="http://www.techempower.com/benchmarks/#section=code">Test type 5: Database updates</a>
 */
public class HelloDbUpdatesResource extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final int DB_ROWS = 10000;

  private static final ObjectMapper mapper = new ObjectMapper();

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    int _queries = attributes.getRequest().getQueryParameters().getParameterValue("queries").toInt(1);
    if (_queries < 1)
    {
      _queries = 1;
    }
    else if (_queries > 500)
    {
      _queries = 500;
    }
    final int queries = _queries;

    final ResourceResponse response = new ResourceResponse();
    response.setContentType("application/json");

    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        Random random = new Random();

        List<World> worldsForJson = new ArrayList<>();

        Session session = HibernateUtil.getSessionFactory().openSession();
        Transaction tx = session.beginTransaction();

        // update in batches. See http://docs.jboss.org/hibernate/core/3.3/reference/en/html/batch.html#batch-update
        ScrollableResults worlds = session.createQuery("from World")
           .setMaxResults(queries)
           .setCacheMode(CacheMode.IGNORE)
           .scroll(ScrollMode.FORWARD_ONLY);
        int count=0;
        while (worlds.next())
        {
          World world = (World) worlds.get(0);
          world.randomNumber = random.nextInt(DB_ROWS) + 1;
          worldsForJson.add(world);
          if ( ++count % 500 == 0 )
          {
            //flush a batch of updates and release memory
            session.flush();
            session.clear();
          }
        }

        tx.commit();
        session.close();

        try
        {
          String data;
          if (queries == 1)
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worldsForJson.get(0));
          }
          else
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worldsForJson);
          }
          attributes.getResponse().write(data);
        }
        catch (IOException ex)
        {
          // do nothing
        }
      }
    });
    return response;
  }
}
