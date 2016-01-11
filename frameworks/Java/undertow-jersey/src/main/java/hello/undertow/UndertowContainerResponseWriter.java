package hello.undertow;

import io.undertow.server.*;
import io.undertow.util.*;
import org.glassfish.jersey.server.*;
import org.glassfish.jersey.server.spi.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class UndertowContainerResponseWriter
    implements ContainerResponseWriter
{
  private HttpServerExchange httpServerExchange;

  public UndertowContainerResponseWriter(
      HttpServerExchange httpServerExchange)
  {
    this.httpServerExchange = httpServerExchange;
  }

  @Override
  public OutputStream writeResponseStatusAndHeaders(long contentLength,
      ContainerResponse responseContext) throws ContainerException
  {
    httpServerExchange.setResponseCode(responseContext.getStatus());

    for (Map.Entry<String, List<String>> mapEntry : responseContext.getStringHeaders().entrySet())
    {
      httpServerExchange.getResponseHeaders().putAll(
          new HttpString(mapEntry.getKey()), mapEntry.getValue());
    }

    httpServerExchange.setResponseContentLength(contentLength);
    return httpServerExchange.getOutputStream();
  }

  @Override
  public boolean suspend(long timeOut, TimeUnit timeUnit,
      TimeoutHandler timeoutHandler)
  {
    return false;
  }

  @Override
  public void setSuspendTimeout(long timeOut, TimeUnit timeUnit)
      throws IllegalStateException
  {
    //        Util.l("suspendtimeout");
  }

  @Override
  public void commit()
  {
    httpServerExchange.endExchange();
  }

  @Override
  public void failure(Throwable error)
  {
    httpServerExchange.setResponseCode(500);
    httpServerExchange.endExchange();
    error.printStackTrace();
  }

  @Override
  public boolean enableResponseBuffering()
  {
    return false;
  }
}
