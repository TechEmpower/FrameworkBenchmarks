package hellowicket;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.AbstractResource;

import java.io.IOException;

public class HelloJsonResponse extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final String HELLO_WORLD = "Hello, World!";
  public static final String APPLICATION_JSON = "application/json";
  public static final ObjectMapper MAPPER = new ObjectMapper();

  static  {
    MAPPER.registerModule(new AfterburnerModule());
  }

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    final ResourceResponse response = new ResourceResponse();
    response.setWriteCallback(new WriteCallback()
    {
      public void writeData(Attributes attributes)
      {
        try
        {
          final WebResponse webResponse = (WebResponse) attributes.getResponse();
          webResponse.setContentLength(27);
          webResponse.setContentType(APPLICATION_JSON);
          JsonMessage message = new JsonMessage(HELLO_WORLD);
          byte[] json = MAPPER.writeValueAsBytes(message);
          webResponse.write(json);
        }
        catch (IOException ex)
        {
          // do nothing
        }
      }
    });
    return response;
  }

  @Override
  protected void setResponseHeaders(final ResourceResponse resourceResponse, final Attributes attributes) {

  }
}
