package hello.accounts.entity;

import com.techempower.gemini.pyxis.*;

/**
 * Represents a User of the GeminiHello application.  Based on Pyxis'
 * BasicWebUser.
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class User
     extends BasicWebUser
{

  //
  // Member variables.
  //

  //
  // Member methods.
  //

  /**
   * Constructor.
   */
  public User(BasicSecurity<User, Group> security)
  {
    super(security);
  }

}   // End User.
