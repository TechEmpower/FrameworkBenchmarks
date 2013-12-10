package hellowicket.dbupdates;

import java.io.IOException;
import java.util.List;
import java.util.Random;

import org.apache.wicket.request.resource.AbstractResource;
import org.hibernate.Query;
import org.hibernate.Session;

import com.fasterxml.jackson.databind.ObjectMapper;

import hellowicket.HibernateUtil;
import hellowicket.World;

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

        Session session = HibernateUtil.getSessionFactory().openSession();
        Query query = session
          .createQuery("from World")
          .setMaxResults(queries);
        List<World> worlds = query.list();
        for (World world : worlds)
        {
          world.randomNumber = random.nextInt(DB_ROWS) + 1;
          session.update(world);
        }

        session.close();

        try
        {
          String data;
          if (queries == 1)
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worlds.get(0));
          }
          else
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worlds);
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
