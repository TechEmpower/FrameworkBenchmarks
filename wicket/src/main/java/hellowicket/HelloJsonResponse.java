package hellowicket;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.wicket.request.resource.AbstractResource;
import com.fasterxml.jackson.databind.*;

public class HelloJsonResponse extends AbstractResource
{
	private static final long serialVersionUID = 1L;
	
	private static final ObjectMapper mapper = new ObjectMapper();

	protected ResourceResponse newResourceResponse(Attributes attributes)
	{
    ResourceResponse response = new ResourceResponse();
    response.setContentType("application/json");
    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        Map<String, String> data = new HashMap<String, String>();
        data.put("message", "Hello, World!");

        try
        {
          attributes.getResponse().write(HelloJsonResponse.mapper.writeValueAsString(data));
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
