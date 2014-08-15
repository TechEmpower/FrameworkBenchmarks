package hello;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * JSON Encoding Test
 */
@SuppressWarnings("serial")
public class JsonServlet extends HttpServlet
{

  // Response message class.
  public static class HelloMessage {
    public final String message = "Hello, World!";
  }

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse res)
      throws ServletException, IOException
  {
    // Set content type to JSON
    res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

    // Write JSON encoded message to the response.
    try
    {
      Common.MAPPER.writeValue(res.getOutputStream(), new HelloMessage());
    }
    catch (IOException ioe) 
    {
      // do nothing
    }
  }
  
}
