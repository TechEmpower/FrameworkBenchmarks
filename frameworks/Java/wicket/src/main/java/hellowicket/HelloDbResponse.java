package hellowicket;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.util.string.StringValue;

import com.fasterxml.jackson.core.JsonProcessingException;

public class HelloDbResponse implements IResource
{
  private static final long serialVersionUID = 1L;

  private static final int DB_ROWS = 10000;
  private static final String TEXT_PLAIN = "text/plain";

  @Override
  public void respond(Attributes attributes) 
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
		
	  try 
	  {
		byte[] data = getDataFromDatabase(queriesParam, queries);

		final WebResponse webResponse = (WebResponse) attributes.getResponse();
		webResponse.setContentLength(data.length);
		webResponse.setContentType(HelloJsonResponse.APPLICATION_JSON);
		webResponse.write(data);
	  } 
	  catch (Exception ex)
	  {
		WebResponse response = (WebResponse) attributes.getResponse();
		
		response.setContentType(TEXT_PLAIN);
		response.setStatus(500);
		response.write(ex.getClass().getSimpleName() + ": " + ex.getMessage());
		
		ex.printStackTrace();
	  }
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
