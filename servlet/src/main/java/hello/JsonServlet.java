package hello;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.fasterxml.jackson.databind.*;

/**
 * JSON Encoding Test
 */
@SuppressWarnings("serial")
public class JsonServlet extends HttpServlet
{
  // Constants for setting the content type.
  private static final String HEADER_CONTENT_TYPE    = "Content-Type";
  private static final String CONTENT_TYPE_JSON      = "application/json";

  // Jackson encoder, reused for each response.
  private final ObjectMapper mapper = new ObjectMapper();

  // Response message class.
  public static class HelloMessage {
    public final String message = "Hello, World!";
  }

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse res)
      throws ServletException, IOException
  {
    // Set content type to JSON
    res.setHeader(HEADER_CONTENT_TYPE, CONTENT_TYPE_JSON);

    // Write JSON encoded message to the response.
    try
    {
      mapper.writeValue(res.getOutputStream(), new HelloMessage());
    }
    catch (IOException ioe) 
    {
      // do nothing
    }
  }
}
