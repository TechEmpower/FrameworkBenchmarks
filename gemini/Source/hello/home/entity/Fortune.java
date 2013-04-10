package hello.home.entity;

import com.techempower.js.*;

import hello.*;

/**
 * A fortune entity.
 */
public class   Fortune
    extends    GhDataEntity
    implements Comparable<Fortune>
{

  private String message;
  
  /**
   * Set the message.  
   */
  public void setMessage(String message)
  {
    this.message = message;
  }
  
  /**
   * Get the message.
   */
  public String getMessage()
  {
    return this.message;
  }

  /**
   * A visitor factory used to map this class to JSON.
   */
  public static final VisitorFactory<Fortune> VISITOR_FACTORY = new VisitorFactory<Fortune>()
  {
    @Override
    public Visitor visitor(Fortune fortune)
    {
      return Visitors.map(
          "id", fortune.getId(),
          "message", fortune.getMessage());
    }
  };

  /**
   * For our purposes, Fortunes sort by their message text. 
   */
  @Override
  public int compareTo(Fortune other)
  {
    return getMessage().compareTo(other.getMessage());
  }
  
}
