package hello.pages;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.tapestry5.*;
import org.apache.tapestry5.json.*;
import org.apache.tapestry5.util.*;

import com.fasterxml.jackson.databind.*;

/**
 * JSON Encoding Test
 */
public class HelloJSON
{
  // Constant for setting the content type.
  private static final String CONTENT_TYPE_JSON = "application/json";
  private static final ObjectMapper mapper = new ObjectMapper();

  StreamResponse onActivate() {
    Map<String, String> data = new HashMap<String, String>();
    data.put("message", "Hello, World!");
    
    String response = "";
    try
    {
      response = HelloJSON.mapper.writeValueAsString(data);
    }
    catch (IOException ex)
    {
      // do nothing
    }
    
    return new TextStreamResponse(CONTENT_TYPE_JSON, response);
  }
}
