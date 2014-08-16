package hellowicket;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.util.string.StringValue;

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

    final ResourceResponse response = new ResourceResponse();
    response.setContentType(CONTENT_TYPE);

    response.setWriteCallback(new WriteCallback()
    {
      public void writeData(Attributes attributes)
      {
        try
        {
          final ThreadLocalRandom random = ThreadLocalRandom.current();
          DataSource dataSource = WicketApplication.get().getDataSource();
          World[] worlds = new World[queries];
          try (Connection connection = dataSource.getConnection())
          {
            try (PreparedStatement statement = connection.prepareStatement(
                       "SELECT * FROM World WHERE id = ?",
                       ResultSet.TYPE_FORWARD_ONLY,
                       ResultSet.CONCUR_READ_ONLY))
            {
              for (int i = 0; i < queries; i++)
              {
                  statement.setInt(1, random.nextInt(DB_ROWS) + 1);
                  try (ResultSet resultSet = statement.executeQuery())
                  {
                      resultSet.next();
                      worlds[i] = new World(
                              resultSet.getInt("id"),
                              resultSet.getInt("randomNumber"));
                  }
              }
            }
          }

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
        catch (Exception ex)
        {
          // do nothing
        }
      }
    });
    return response;
  }
}
