package hello;

/**
 * Simple Fortune cookie entity.
 */
public class Fortune
  implements Comparable<Fortune>
{

  private final int    id;
  private final String message;
  
  public Fortune(int id, String message)
  {
    this.id = id;
    this.message = message;
  }
  
  public int getId()
  {
    return this.id;
  }
  
  public String getMessage()
  {
    return this.message;
  }      

  /**
   * For our purposes, Fortunes sort by their message text. 
   */
  @Override
  public int compareTo(Fortune other)
  {
    return getMessage().compareTo(other.getMessage());
  }
  
}
