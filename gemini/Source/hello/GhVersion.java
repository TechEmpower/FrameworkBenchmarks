package hello;

import com.techempower.*;
import com.techempower.gemini.*;
import com.techempower.helper.*;

/**
 * Provides a name, client, and version number for the current build of 
 * the GeminiHello application.
 *
 * @see com.techempower.Version
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhVersion
     extends Version
  implements GeminiConstants
{

  //
  // Member methods.
  //

  /**
   * Constructor.  This builds the version string.
   */
  public GhVersion()
  {
    this.setVersionString(getMajorVersion() + "." 
        + StringHelper.padZero(getMinorVersion(), 2)
        + "(" + StringHelper.padZero(getMicroVersion(), 2) 
        + ") (Gemini " + GEMINI_VERSION + ")");
  }

  /**
   * Get the version levels.
   */
  @Override
  public int getMajorVersion()  { return 0; }
  @Override
  public int getMinorVersion()  { return 1; }
  @Override
  public int getMicroVersion()  { return 0; }

  /**
   * Gets the product code.
   */
  @Override
  public String getProductCode()
  {
    return "GH";
  }

  /**
   * Gets the product name.
   */
  @Override
  public String getProductName()
  {
    return "GeminiHello";
  }

  /**
   * Gets the client's name.
   */
  @Override
  public String getClientName()
  {
    return "TechEmpower, Inc.";
  }

  /**
   * Gets the developer's name.
   */
  @Override
  public String getDeveloperName()
  {
    return "TechEmpower, Inc.";
  }

  /**
   * Gets the copyright years.
   */
  @Override
  public String getCopyrightYears()
  {
    return "----";
  }

}   // End GhVersion.

