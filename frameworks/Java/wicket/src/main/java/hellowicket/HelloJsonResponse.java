package hellowicket;

import java.io.IOException;

import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.IResource;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

public class HelloJsonResponse implements IResource
{
  private static final long serialVersionUID = 1L;

  private static final String HELLO_WORLD = "Hello, World!";
  public static final String APPLICATION_JSON = "application/json";
  public static final ObjectMapper MAPPER = new ObjectMapper();

  static  {
    MAPPER.registerModule(new AfterburnerModule());
  }

  @Override
  public void respond(Attributes attributes) 
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
}
