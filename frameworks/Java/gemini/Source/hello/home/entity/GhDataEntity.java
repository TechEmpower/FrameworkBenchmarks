package hello.home.entity;

import com.techempower.util.*;

/**
 * The base class for all data entities in GeminiHello.  This class provides
 * the implementation of the Identifiable interface so that sub-classes
 * only need to provide member variables and typical get/set methods.
 *
 * Development history:
 *   2012-04-19 - mh - Class created
 *
 * @author mhixson
 */
public abstract class GhDataEntity
           implements Identifiable
{
  /**
   * The identity for this object.
   */
  private int id;

  @Override
  public int getId()
  {
    return this.id;
  }

  @Override
  public void setId(int newIdentity)
  {
    this.id = newIdentity;
  }

  @Override
  public String toString()
  {
    return getClass().getSimpleName() + "[" + getId() + "]";
  }

}   // End GhDataEntity.
