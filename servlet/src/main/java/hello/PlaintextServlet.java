package hello;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Plaintext rendering Test
 */
@SuppressWarnings("serial")
public class PlaintextServlet extends HttpServlet
{

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse res)
      throws ServletException, IOException
  {
    // Set content type to text/plain.
    res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_TEXT);

    // Write plaintext "Hello, World!" to the response.
    try
    {
      res.getWriter().write("Hello, World!");
    }
    catch (IOException ioe) 
    {
      // do nothing
    }
  }
  
}
