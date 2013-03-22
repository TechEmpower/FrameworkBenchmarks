package hello;

import com.techempower.gemini.*;
import com.techempower.gemini.jsp.*;
import com.techempower.util.*;

/**
 * Provides a GeminiHello-specific Web Site Infrastructure.
 *
 * @see com.techempower.gemini.BasicInfrastructure
 * @see com.techempower.gemini.Infrastructure
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhInfrastructure
     extends BasicInfrastructure
{
  //
  // Member variables.
  //

  private boolean   scheduledEvents = false;

  //
  // Member methods.
  //

  /**
   * Constructor.  This GhInfrastructure creates a Scheduler
   * for running Events.
   */
  public GhInfrastructure(GeminiApplication application)
  {
    super(application);

    // Construct instances of application-specific Event subclasses.
  }

  // @see com.techempower.gemini.BasicInfrastructure#configureSas(com.techempower.gemini.jsp.ScriptsAndSheets)
  @Override
  protected void configureSas(ScriptsAndSheets applicationSas)
  {
    // Specify application-scope JavaScript and CSS dependencies.  These
    // scripts and sheets are to be included on -all- pages rendered by
    // the application.  Note that the configuration set up below by the
    // application template assumes that no unification and minification
    // are happening within the Test and Production environments.
    
    // TODO: If CSS and JS are being concatenated and/or minified for
    // test and production, modify the following accordingly.
    applicationSas.addSheet("gh.css");
    applicationSas.addScript("jquery.js");
    applicationSas.addScript("gh.js");
    applicationSas.addScript("gh.forms.js");

    // TODO: Change this to be an application-specific icon.
    applicationSas.setFavicon("alert.gif");
  }

  /**
   * Configure this component.  (Called by BasicInfrastructure.configure.)
   */
  @Override
  public void customConfigure(EnhancedProperties props)
  {
    // Does nothing.
  }

  /**
   * Called by BasicInfrastructure's begin() method.  BasicInfrastructure.
   * ensures that this method will only be called if the Infrastructure is
   * not already started.
   */
  @Override
  public void start()
  {
    // Add the standard events if they haven't already been added.
    if (!this.scheduledEvents)
    {
    	/*
    	// Example:
      eventMonthlyEmail = new EmployeeStatusEvent(application);
      scheduler.scheduleEvent(eventMonthlyEmail);
      */

      this.scheduledEvents = true;
    }
  }

  /**
   * Called by BasicInfrastructure's end() method.  BasicInf. ensures
   * that this method will only be called if the Infrastructure is not
   * already shut down.
   */
  @Override
  public void shutdown()
  {
    // Do anything you need to at application shut-down time.
  }

}   // End GhInfrastructure.

