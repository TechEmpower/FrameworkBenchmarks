package hello.home.entity;

import com.techempower.data.annotation.*;
import com.techempower.js.*;

/**
 * Simple World entity.
 */
@Entity
public class   World
    extends    GhDataEntity
{

  private int randomNumber;
  
  /**
   * Set the random number.  
   */
  public void setRandomNumber(int randomNumber)
  {
    this.randomNumber = randomNumber;
  }
  
  /**
   * Get the random number.
   */
  public int getRandomNumber()
  {
    return this.randomNumber;
  }

  /**
   * A visitor factory used to map this class to JSON.
   */
  public static final VisitorFactory<World> VISITOR_FACTORY = new VisitorFactory<World>()
  {
    @Override
    public Visitor visitor(World world)
    {
      return Visitors.map(
          "id", world.getId(),
          "randomNumber", world.getRandomNumber());
    }
  }; 
  
}
