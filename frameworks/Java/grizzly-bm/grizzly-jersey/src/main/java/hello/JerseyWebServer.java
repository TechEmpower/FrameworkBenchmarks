package hello;

import com.sun.jersey.api.container.grizzly2.GrizzlyServerFactory;
import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.api.core.ResourceConfig;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import javax.ws.rs.core.UriBuilder;
import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.glassfish.grizzly.Grizzly;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.HttpServer;

public class JerseyWebServer {

  public static void main(String[] args) throws Exception {
    CommandLineParser parser = new BasicParser();
    CommandLine cmd = parser.parse(options(), args);

    int port = Integer.parseInt(cmd.getOptionValue("port", "8080"));
    String dbHost = cmd.getOptionValue("dbhost", "localhost");
    int dbPort = Integer.parseInt(cmd.getOptionValue("dbport", "3306"));

    new JerseyWebServer(port,dbHost, dbPort).run();
  }

  private final int port;
  private final String dbHost;
  private final int dbPort;

  public JerseyWebServer(int port, String dbHost, int dbPort) {
    this.port = port;
    this.dbHost = dbHost;
    this.dbPort = dbPort;
  }

  public void run() throws Exception {
    URI baseUri = getBaseUrl(port);
    ResourceConfig rc = new PackagesResourceConfig("hello");
    rc.setPropertiesAndFeatures(properties());
    rc.getContainerResponseFilters().add(new ServerHeaderFilter());
    HttpServer server = GrizzlyServerFactory.createHttpServer(baseUri, rc);

    // There will be *a lot* of broken connections during the plaintext test.
    // That's not a good thing, but what would make matters even worse would be
    // to log the full stack trace of an IOException for each broken connection.
    // That's what Grizzly does by default, and it logs those messages at the
    // WARNING level, so setting the threshold to SEVERE hides those messages.
    Grizzly.logger(HttpHandler.class).setLevel(Level.SEVERE);

    try {
        server.start();
        System.err.print("Server started.\n");
        synchronized (JerseyWebServer.class) {
            JerseyWebServer.class.wait();
        }
    } finally {
        server.stop();
    }
  }

  private Map<String, Object> properties() {
    Map<String, Object> properties = new HashMap<>();
    properties.put("dbhost", dbHost);
    properties.put("dbport", dbPort);
    return properties;
  }

  private static URI getBaseUrl(int port) {
    return UriBuilder.fromUri("http://0.0.0.0/").port(port).build();
  }

  private static Options options() {
    Options options = new Options();
    options.addOption("port", true, "server port");
    options.addOption("dbhost", true, "database host");
    options.addOption("dbport", true, "database port");
    return options;
  }
}
