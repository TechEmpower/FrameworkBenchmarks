package hellowicket.dbupdates;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import org.apache.wicket.request.resource.AbstractResource;

import com.fasterxml.jackson.databind.ObjectMapper;

import hellowicket.WicketApplication;
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
        try
        {
          final ThreadLocalRandom random = ThreadLocalRandom.current();
          DataSource dataSource = WicketApplication.get().getDataSource();

          World[] worlds = new World[queries];
          try (Connection connection = dataSource.getConnection();
               PreparedStatement query = connection.prepareStatement(
                       "SELECT * FROM World WHERE id = ?",
                       ResultSet.TYPE_FORWARD_ONLY,
                       ResultSet.CONCUR_READ_ONLY);
               PreparedStatement update = connection.prepareStatement(
                       "UPDATE World SET randomNumber = ? WHERE id= ?"))
          {
            for (int i = 0; i < queries; i++)
            {
              query.setInt(1, random.nextInt(DB_ROWS) + 1);
              World world;
              try (ResultSet resultSet = query.executeQuery())
              {
                resultSet.next();
                world = new World(
                    resultSet.getInt("id"),
                    resultSet.getInt("randomNumber"));
              }
              world.randomNumber = random.nextInt(DB_ROWS) + 1;
              update.setInt(1, world.randomNumber);
              update.setInt(2, world.id);
              update.executeUpdate();
              worlds[i] = world;
            }
          }

          String data;
          if (queries == 1)
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worlds[0]);
          }
          else
          {
              data = HelloDbUpdatesResource.mapper.writeValueAsString(worlds);
          }
          attributes.getResponse().write(data);
        }
        catch (Exception ex)
        {
          // do nothing
        }
      }
    });
    return response;
  }
}
