package hello.undertow;

import io.undertow.server.*;
import io.undertow.util.*;
import org.glassfish.jersey.internal.*;
import org.glassfish.jersey.server.*;
import org.glassfish.jersey.server.spi.*;

import javax.ws.rs.core.*;
import java.net.*;
import java.security.*;

public class UndertowJerseyContainer
    implements Container, HttpHandler
{

  /**
   * Default dummy security context.
   * Taken from Jersey's ApplicationHandler class
   */
  private static final SecurityContext DEFAULT_SECURITY_CONTEXT = new SecurityContext()
  {

    @Override
    public boolean isUserInRole(final String role)
    {
      return false;
    }

    @Override
    public boolean isSecure()
    {
      return false;
    }

    @Override
    public Principal getUserPrincipal()
    {
      return null;
    }

    @Override
    public String getAuthenticationScheme()
    {
      return null;
    }
  };
  private ApplicationHandler applicationHandler;

  public UndertowJerseyContainer(Application configuration)
  {
    this.applicationHandler = new ApplicationHandler(configuration);
  }

  @Override
  public ResourceConfig getConfiguration()
  {
    return this.applicationHandler.getConfiguration();
  }

  @Override
  public ApplicationHandler getApplicationHandler()
  {
    return this.applicationHandler;
  }

  @Override
  public void reload()
  {
    reload(this.applicationHandler.getConfiguration());
  }

  @Override
  public void reload(ResourceConfig configuration)
  {

    this.applicationHandler.onShutdown(this);

    this.applicationHandler = new ApplicationHandler(configuration);

    this.applicationHandler.onStartup(this);
    this.applicationHandler.onReload(this);
  }

  @Override
  public void handleRequest(final HttpServerExchange httpServerExchange)
      throws Exception
  {
    String requestUri = httpServerExchange.getRequestURI();
    if(httpServerExchange.getQueryString().length() > 0)
    {
      requestUri = requestUri + "?" + httpServerExchange.getQueryString();
    }

    final ContainerRequest request = new ContainerRequest(null,
        URI.create(requestUri),
        httpServerExchange.getRequestMethod().toString(),
        DEFAULT_SECURITY_CONTEXT, new MapPropertiesDelegate());

    request.setWriter(
        new UndertowContainerResponseWriter(httpServerExchange));

    for (HeaderValues values : httpServerExchange.getRequestHeaders())
    {
      request.headers(values.getHeaderName().toString(), values);
    }

    request.setEntityStream(httpServerExchange.getInputStream());
    this.applicationHandler.handle(request);
  }
}
