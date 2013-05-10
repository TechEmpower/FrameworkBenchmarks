package hello;

import com.sun.jersey.spi.container.ContainerRequest;
import com.sun.jersey.spi.container.ContainerResponse;
import com.sun.jersey.spi.container.ContainerResponseFilter;

public class ServerResponseFilter implements ContainerResponseFilter {
  
  private static final String SERVER_HEADER = "Server";
  private static final String SERVER_VALUE  = "Grizzly";
  
  @Override
  public ContainerResponse filter(final ContainerRequest request, final ContainerResponse response) {
    response.getHttpHeaders().add(SERVER_HEADER, SERVER_VALUE);
    return response;
  }

}
