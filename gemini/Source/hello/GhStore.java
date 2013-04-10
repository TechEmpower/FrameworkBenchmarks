package hello;

import hello.home.entity.*;

import com.techempower.*;
import com.techempower.cache.*;
import com.techempower.data.*;
import com.techempower.log.*;

/**
 * GhStore provides data entity services.
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhStore
     extends EntityStore
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
  public GhStore(TechEmpowerApplication application, ConnectorFactory
      connectorFactory)
  {
    super(application, connectorFactory);
  }

  /**
   * Initializes the EntityStore.
   */
  @Override
  public void initialize()
  {
    getLog().debug("Registering Entity Store groups.", LogLevel.DEBUG);

    // Register entities.
    
    // Use EntityGroup rather than CacheGroup to ensure World entities are not cached.
    register(EntityGroup.of(World.class));
    register(EntityGroup.of(Fortune.class));

    // Register relationships.
    // We have no relationships in this application.

    // Add handlers for clustering.
    // We're not using clustering in this application.
    /*
    GhApplication application = (GhApplication)super.getApplication();
    if (application.getClusterClient().isEnabled())
    {
      CacheHandler cacheHandler = new CacheHandler(application);
      application.getClusterClient().addHandler(cacheHandler);
      this.addListener(cacheHandler);

      CachedRelationHandler cachedRelationHandler = new CachedRelationHandler(application);
      application.getClusterClient().addHandler(cachedRelationHandler);
      for (CachedRelation<?, ?> relation : this.getRelations())
      {
        relation.addListener(cachedRelationHandler);
      }
    }
    */
  }

}   // End GhStore.
