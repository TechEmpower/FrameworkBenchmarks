package hellowicket;

import java.io.IOException;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.wicket.request.resource.AbstractResource;
import org.hibernate.Session;

import com.fasterxml.jackson.databind.ObjectMapper;

public class HelloDbResponse extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final int DB_ROWS = 10000;

  private static final ObjectMapper mapper = new ObjectMapper();

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    final int queries = attributes.getRequest().getQueryParameters().getParameterValue("queries").toInt(1);
    final World[] worlds = new World[queries];
    final ThreadLocalRandom random = ThreadLocalRandom.current();

    final ResourceResponse response = new ResourceResponse();
    response.setContentType("application/json");

    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        final Session session = HibernateUtil.getSessionFactory().openSession();

        for (int i = 0; i < queries; i++)
        {
          worlds[i] = (World)session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
        }

        session.close();

        try
        {
          String data;
          if (queries == 1)
          {
              data = HelloDbResponse.mapper.writeValueAsString(worlds[0]);
          }
          else
          {
              data = HelloDbResponse.mapper.writeValueAsString(worlds);
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
