package hello;

import hello.accounts.entity.*;

import com.techempower.gemini.*;
import com.techempower.gemini.pyxis.*;

/**
 * GhSecurity provides Pyxis-based security services for the
 * GeminiHello application.
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhSecurity
     extends BasicSecurity<User, Group>
{

  //
  // Member methods.
  //

  /**
   * Constructor.
   */
  public GhSecurity(GeminiApplication application)
  {
    super(application, User.class, Group.class);
  }
  
  /**
   * Constructs a PyxisUser object or a subclass thereof.  Applications
   * should overload this method to return an instance of a BasicUser
   * subclass.
   */
  @Override
  public User constructUser()
  {
    return new User(this);
  }

  /**
   * Gets the logged-in user from the Context's session.  Returns null
   * if no user is logged in.
   *
   * @param Context the Context from which to retrieve a user.
   */
  @Override
  public User getUser(Context context)
  {
    return (User)super.getUser(context);
  }

}   // End GhSecurity.
