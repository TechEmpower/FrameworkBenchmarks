package hello;

import javax.servlet.annotation.*;

import com.techempower.gemini.transport.*;

/**
 * Main Servlet to be used by the GeminiHello application.  Upon
 * receiving a request, this Servlet creates a Context object and
 * then invokes the Dispatcher.  The Dispatcher determines what happens
 * next.
 *
 * @see com.techempower.gemini.transport.InfrastructureServlet
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
@SuppressWarnings("serial")
@WebServlet(name="Gh", urlPatterns="*")
public class GhServlet
     extends InfrastructureServlet
{

  //
  // Public member methods.
  //

  /**
   * Gets a GeminiApplication object for this application.
   */
  @Override
  public GhApplication getApplication()
  {
    return GhApplication.getInstance();
  }

}   // End GhServlet.

