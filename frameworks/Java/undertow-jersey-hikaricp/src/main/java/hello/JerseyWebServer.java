package hello;

import hello.undertow.*;
import io.undertow.*;
import io.undertow.server.handlers.*;
import org.apache.commons.cli.*;
import org.glassfish.hk2.utilities.binding.*;
import org.glassfish.jersey.server.*;
import org.hibernate.*;

import javax.inject.*;
import java.util.*;

public class JerseyWebServer
{
  private final int    port;
  private final String dbHost;
  private final int    dbPort;

  public JerseyWebServer(final int port, final String dbHost,
      final int dbPort)
  {
    this.port = port;
    this.dbHost = dbHost;
    this.dbPort = dbPort;
  }

  public static void main(final String[] args) throws Exception
  {
    CommandLineParser parser = new BasicParser();
    CommandLine cmd = parser.parse(options(), args);

    final int port = Integer.parseInt(cmd.getOptionValue("port", "8080"));
    final String dbHost = cmd.getOptionValue("dbhost", "localhost");
    final int dbPort = Integer.parseInt(cmd.getOptionValue("dbport", "3306"));

    ResourceConfig config = new ResourceConfig(DbResource.class,
        FortunesResource.class, JsonResource.class,
        JsonMessageBodyWriter.class, ServerResponseFilter.class, RequestExceptionMapper.class);

    config.setProperties(new HashMap<String, Object>()
    {{
      put("dbhost", dbHost);

      put("dbport", dbPort);
    }});

    config.register(org.glassfish.jersey.server.mvc.MvcFeature.class);
    config.register(
        org.glassfish.jersey.server.mvc.mustache.MustacheMvcFeature.class);

    config.register(new AbstractBinder()
    {
      @Override
      protected void configure()
      {
        bindFactory(SessionFactoryProvider.class).to(SessionFactory.class).in(
          Singleton.class);
      }
    });

    UndertowJerseyContainer container = new UndertowJerseyContainer(config);

    BlockingHandler bh = new BlockingHandler(container);

    Undertow server = Undertow.builder().addHttpListener(port,
        "0.0.0.0").setHandler(bh).build();
    server.start();
  }

  private static Options options()
  {
    Options options = new Options();
    options.addOption("port", true, "server port");
    options.addOption("dbhost", true, "database host");
    options.addOption("dbport", true, "database port");
    return options;
  }
}
