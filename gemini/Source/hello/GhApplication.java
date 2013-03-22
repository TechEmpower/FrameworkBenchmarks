package hello;

import hello.home.entity.*;
import hello.home.handler.*;

import com.techempower.*;
import com.techempower.cache.*;
import com.techempower.data.*;
import com.techempower.gemini.*;
import com.techempower.gemini.cluster.client.*;
import com.techempower.gemini.cluster.client.handler.*;
import com.techempower.gemini.data.*;
import com.techempower.gemini.exceptionhandler.*;
import com.techempower.gemini.path.*;
import com.techempower.gemini.pyxis.*;
import com.techempower.js.*;

/**
 * GeminiHello Application.  As a subclass of GeminiApplication, this
 * class acts as the central "hub" of references to components such as
 * the Dispatcher and EntityStore.
 *
 * @see com.techempower.gemini.GeminiApplication
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *
 * @author mhixson
 */
public class GhApplication
     extends GeminiApplication
  implements PyxisApplication
{
  //
  // Static variables.
  //

  private static final GhApplication instance = new GhApplication();

  //
  // Member methods.
  //

  /**
   * Constructor.  This method can be extended to construct references
   * to custom components for the application.
   */
  public GhApplication()
  {
    super();

    // Add the GhInfrastructure as an Asynchronous resource
    // that should be stopped and started along with the application.
    addAsynchronous(getInfrastructure());
  }

  /**
   * Constructs an application-specific Context, for an inbound request.
   * This is overloaded to return a custom GhContext.
   */
  @Override
  public GhContext getContext(Request request)
  {
    return new GhContext(
        request,
        this
    );
  }

  /**
   * Returns an application-specific reference to the Security manager.
   */
  @Override
  public GhSecurity getSecurity()
  {
    return (GhSecurity)super.getSecurity();
  }

  /**
   * Returns an application-specific reference to the EntityStore.
   */
  @Override
  public GhStore getStore()
  {
    return (GhStore)super.getStore();
  }

  /**
   * Returns an application-specific reference to the Infrastructure.
   */
  @Override
  public GhInfrastructure getInfrastructure()
  {
    return (GhInfrastructure)super.getInfrastructure();
  }

  //
  // Protected component constructors.
  //

  /**
   * Constructs a Version reference.  This is overloaded to return a
   * custom GhVersion object.
   */
  @Override
  protected Version constructVersion()
  {
    return new GhVersion();
  }

  /**
   * Construct a Security reference.
   */
  @Override
  protected PyxisSecurity constructSecurity()
  {
    return new GhSecurity(this);
  }

  /**
   * Constructs an EntityStore reference.  Overload to return a custom
   * object.
   *   <p>
   * Note: it is acceptable to return null if no object caching is used.
   * The default implementation does exactly that.
   */
  @Override
  protected EntityStore constructEntityStore()
  {
    return new GhStore(this, getConnectorFactory());
  }

  /**
   * Constructs the application's Cluster Client.
   */
  @Override
  protected ClusterClient constructClusterClient()
  {
    ClusterClient client = super.constructClusterClient();
    client.addHandler(new LogNoteHandler(this));
    return client;
  }

  /**
   * Constructs a Infrastructure reference.  This is overloaded to return a
   * custom GhInfrastructure object.
   */
  @Override
  protected BasicInfrastructure constructInfrastructure()
  {
    return new GhInfrastructure(this);
  }

  /**
   * Creates a JavaScriptWriter for writing JSON.
   */
  @Override
  protected JavaScriptWriter constructJavaScriptWriter()
  {
    return JavaScriptWriter.custom()
        .addVisitorFactory(World.class, World.VISITOR_FACTORY)
        .build();
  }

  /**
   * Constructs a Dispatcher.
   */
  @Override
  protected Dispatcher constructDispatcher()
  {
    final PathDispatcher.Configuration<GhContext> config = new PathDispatcher.Configuration<>();

    config.setDefault(new HelloHandler(this))
          .add(new BasicExceptionHandler(this));

    return new PathDispatcher<>(this, config);
  }

  /**
   * Construct a DatabaseConnectionListener.  If this returns non-null,
   * this listener object will be provided to the ConnectorFactory instance
   * upon its creation.
   */
  @Override
  protected DatabaseConnectionListener constructDatabaseConnectionListener()
  {
    return new BasicConnectorListener(this);
  }

  //
  // Static methods.
  //

  public static GhApplication getInstance()
  {
    return instance;
  }

}   // End GhApplication.

