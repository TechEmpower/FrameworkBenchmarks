package hello;

import javax.ws.rs.container.*;
import java.io.*;

public class ServerResponseFilter
    implements ContainerResponseFilter
{

  private static final String SERVER_HEADER = "Server";
  private static final String SERVER_VALUE  = "Undertow";

  @Override
  public void filter(ContainerRequestContext requestContext,
      ContainerResponseContext responseContext) throws IOException
  {
    responseContext.getHeaders().add(SERVER_HEADER, SERVER_VALUE);
  }
}
