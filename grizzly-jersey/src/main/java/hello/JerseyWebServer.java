package hello;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.core.UriBuilder;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.glassfish.grizzly.http.server.HttpServer;

import com.sun.jersey.api.container.grizzly2.GrizzlyServerFactory;
import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.api.core.ResourceConfig;

public class JerseyWebServer {

  private final int port;
  private final String dbHost;
  private final int dbPort;

  public JerseyWebServer(final int port, final String dbHost, final int dbPort) {
    this.port = port;
    this.dbHost = dbHost;
    this.dbPort = dbPort;
  }

  public void run() throws Exception {
    URI baseUri = getBaseUrl(port);
    ResourceConfig rc = new PackagesResourceConfig("hello", "org.eluder.jersey.mustache");
    rc.setPropertiesAndFeatures(properties());
    rc.getContainerResponseFilters().add(new ServerResponseFilter());
    HttpServer server = GrizzlyServerFactory.createHttpServer(baseUri, rc);
    
    System.err.print("Server started. Press ENTER to stop.");
    System.in.read();
    server.stop();
  }

  private Map<String, Object> properties() {
    Map<String, Object> properties = new HashMap<String, Object>();
    properties.put("dbhost", dbHost);
    properties.put("dbport", dbPort);
    return properties;
  }
  
  private static URI getBaseUrl(final int port) {
    return UriBuilder.fromUri("http://0.0.0.0/").port(port).build();
  }

  public static void main(final String[] args) throws Exception {
    CommandLineParser parser = new BasicParser();
    CommandLine cmd = parser.parse(options(), args);
    
    int port = Integer.parseInt(cmd.getOptionValue("port", "8080"));
    String dbHost = cmd.getOptionValue("dbhost", "localhost");
    int dbPort = Integer.parseInt(cmd.getOptionValue("dbport", "3306"));
    
    new JerseyWebServer(port,dbHost, dbPort).run();
  }
  
  private static Options options() {
    Options options = new Options();
    options.addOption("port", true, "server port");
    options.addOption("dbhost", true, "database host");
    options.addOption("dbport", true, "database port");
    return options;
  }
}
