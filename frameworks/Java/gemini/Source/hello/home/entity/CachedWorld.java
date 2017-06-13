package hello.home.entity;

import com.techempower.data.annotation.*;
import com.techempower.js.legacy.*;

/**
 * Simple World entity.
 */
@CachedEntity(table="world")
public class   CachedWorld
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
  public static final VisitorFactory<CachedWorld> VISITOR_FACTORY = new VisitorFactory<CachedWorld>()
  {
    @Override
    public Visitor visitor(CachedWorld world)
    {
      return Visitors.map(
        "id", world.getId(),
        "randomNumber", world.getRandomNumber());
    }
  };

}
