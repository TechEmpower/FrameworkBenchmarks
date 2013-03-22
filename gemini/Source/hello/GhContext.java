package hello;

import com.techempower.gemini.*;

/**
 * A special version of Context that provides GeminiHello-specific
 * functionality.
 *
 * @see com.techempower.gemini.Context
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhContext
     extends Context
{

  //
  // Member variables.
  //

  //
  // Member methods.
  //

  /**
   * Standard constructor.
   *
   * @param request the Request object received by the servlet.
   * @param application the application.
   */
  public GhContext(Request request,
      GeminiApplication application)
  {
    super(request, application);
  }

  /**
   * @see com.techempower.gemini.Context.getApplication()
   */
  @Override
  public GhApplication getApplication()
  {
    return (GhApplication)super.getApplication();
  }

}   // End GhContext.

