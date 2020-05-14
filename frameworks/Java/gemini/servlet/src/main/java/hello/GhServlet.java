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
 *   2020-04-17 - ms - Updated to Gemini 3.0.2
 *
 * @author mhixson
 */
@SuppressWarnings("serial")
@WebServlet(name="Gh", urlPatterns="*")
public class GhServlet
     extends InfrastructureServlet
{
  public static final GhApplication APP_INSTANCE = new GhApplication();

  /**
   * Gets a GeminiApplication object for this application.
   */
  @Override
  public GhApplication getApplication()
  {
    return APP_INSTANCE;
  }

}
