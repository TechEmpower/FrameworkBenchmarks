package hellowicket;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.util.string.StringValue;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

public class HelloDbResponse extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final int DB_ROWS = 10000;
  private static final String TEXT_PLAIN = "text/plain";

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

    try
    {
      final byte[] data = getDataFromDatabase(queriesParam, queries);
      final WebResponse webResponse = (WebResponse) attributes.getResponse();
      webResponse.setContentLength(data.length);
      webResponse.setContentType(HelloJsonResponse.APPLICATION_JSON);
      response.setWriteCallback(new WriteCallback()
      {
        public void writeData(Attributes attributes)
        {
          webResponse.write(data);
        }
      });
    }
    catch (Exception ex)
    {
      response.setContentType(TEXT_PLAIN);
      response.setError(500, ex.getClass().getSimpleName() + ": " + ex.getMessage());
      ex.printStackTrace();
    }
    return response;
  }

  @Override
  protected void setResponseHeaders(final ResourceResponse resourceResponse, final Attributes attributes) {
  }

  private byte[] getDataFromDatabase(final StringValue queriesParam, final int queries)
      throws SQLException, JsonProcessingException
  {
    final ThreadLocalRandom random = ThreadLocalRandom.current();
    DataSource dataSource = WicketApplication.get().getDataSource();
    World[] worlds = new World[queries];
    try (Connection connection = dataSource.getConnection())
    {
      try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM World WHERE id = ?",
          ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY))
      {
        for (int i = 0; i < queries; i++)
        {
          statement.setInt(1, random.nextInt(DB_ROWS) + 1);
          try (ResultSet resultSet = statement.executeQuery())
          {
            resultSet.next();
            worlds[i] = new World(resultSet.getInt("id"), resultSet.getInt("randomNumber"));
          }
        }
      }
    }

    byte[] data;
    if (queriesParam.isNull())
    {
      // request to /db should return JSON object
      data = HelloJsonResponse.MAPPER.writeValueAsBytes(worlds[0]);
    }
    else
    {
      // request to /db?queries=xyz should return JSON array (issue #648)
      data = HelloJsonResponse.MAPPER.writeValueAsBytes(worlds);
    }
    return data;
  }
}
