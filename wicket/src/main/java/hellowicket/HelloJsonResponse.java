package hellowicket;

import java.io.IOException;
import java.util.Map;

import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.util.collections.MiniMap;

import com.fasterxml.jackson.databind.ObjectMapper;

public class HelloJsonResponse extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final String CONTENT_TYPE = "application/json";
  private static final ObjectMapper mapper = new ObjectMapper();

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    final ResourceResponse response = new ResourceResponse();
    response.setContentLength(26);
    response.setContentType(CONTENT_TYPE);
    response.setWriteCallback(new WriteCallback()
    {
      public void writeData(Attributes attributes)
      {
        Map<String, String> data = new MiniMap<>(1);
        data.put("message", "Hello, world");

        try
        {
            String json = HelloJsonResponse.mapper.writeValueAsString(data);
            attributes.getResponse().write(json);
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
