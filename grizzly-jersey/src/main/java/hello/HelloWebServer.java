package hello;

import java.net.URI;

import javax.ws.rs.core.UriBuilder;

import com.sun.jersey.api.container.grizzly2.GrizzlyServerFactory;
import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.api.core.ResourceConfig;

public class HelloWebServer {

  private final int port;

  public HelloWebServer(int port) {
    this.port = port;
  }

  public void run() throws Exception {
    URI baseUri = getBaseUrl(port);
    ResourceConfig rc = new PackagesResourceConfig("hello");
    GrizzlyServerFactory.createHttpServer(baseUri, rc);
    
    System.err.print("Server started. Press ENTER to stop.");
    System.in.read();
  }

  private static URI getBaseUrl(int port) {
    return UriBuilder.fromUri("http://0.0.0.0/").port(port).build();
  }

  public static void main(String[] args) throws Exception {
    int port;
    if (args.length > 0) {
      port = Integer.parseInt(args[0]);
    } else {
      port = 8080;
    }
    new HelloWebServer(port).run();
  }
}
