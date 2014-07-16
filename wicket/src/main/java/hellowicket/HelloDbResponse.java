package hellowicket;

import java.io.IOException;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.util.string.StringValue;
import org.hibernate.IdentifierLoadAccess;
import org.hibernate.Session;

import com.fasterxml.jackson.databind.ObjectMapper;

public class HelloDbResponse extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final int DB_ROWS = 10000;

  private static final String CONTENT_TYPE = "application/json";
  private static final ObjectMapper mapper = new ObjectMapper();

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    final StringValue queriesParam = attributes.getRequest().getQueryParameters().getParameterValue("queries");
    int qs = queriesParam.toInt(1);
    if (qs < 1)
    {
      qs = 1;
    } 
    else if (qs > 500)
    {
      qs = 500;
    }
    final int queries = qs;
    final World[] worlds = new World[queries];
    final ThreadLocalRandom random = ThreadLocalRandom.current();

    final ResourceResponse response = new ResourceResponse();
    response.setContentType(CONTENT_TYPE);

    response.setWriteCallback(new WriteCallback()
    {
      public void writeData(Attributes attributes)
      {
        final Session session = HibernateUtil.getSessionFactory().openSession();

        IdentifierLoadAccess loader = session.byId(World.class);
        for (int i = 0; i < queries; i++)
        {
          worlds[i] = (World) loader.load(random.nextInt(DB_ROWS) + 1);
        }

        session.close();

        try
        {
          String data;
          if (queriesParam.isNull())
          {
              // request to /db should return JSON object
              data = HelloDbResponse.mapper.writeValueAsString(worlds[0]);
          }
          else
          {
              // request to /db?queries=xyz should return JSON array (issue #648)
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
