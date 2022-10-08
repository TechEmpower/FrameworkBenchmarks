package benchmark;

import benchmark.repository.DbFactory;
import gg.jte.ContentType;
import gg.jte.TemplateEngine;
import io.javalin.Javalin;
import io.javalin.rendering.template.JavalinJte;
import java.util.Collections;
import java.util.List;
import io.javalin.http.servlet.DefaultTasks;
import io.javalin.util.ConcurrencyUtil;
import org.eclipse.jetty.http.UriCompliance;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;

public class Main {

    public static void main(String[] args) {

        TemplateEngine templateEngine = TemplateEngine.createPrecompiled(ContentType.Html);
        templateEngine.setTrimControlStructures(true);
        JavalinJte.init(templateEngine, c -> false);

        CustomJsonMapper jsonMapper = new CustomJsonMapper();

        DatabaseController postgres = new DatabaseController(jsonMapper, DbFactory.INSTANCE.getDbService(DbFactory.DbType.POSTGRES));
        DatabaseController mongo = new DatabaseController(jsonMapper, DbFactory.INSTANCE.getDbService(DbFactory.DbType.MONGODB));

        Javalin app = Javalin.create(config -> {
            config.compression.none();
            config.jsonMapper(jsonMapper);
            config.jetty.server(Main::createServer);
            config.pvt.servletRequestLifecycle = List.of(DefaultTasks.INSTANCE.getHTTP());
        }).start(8080);

        app.get("/plaintext", ctx -> ctx.result("Hello, World!"));
        app.get("/json", ctx -> {
            jsonMapper.writeJson(Collections.singletonMap("message", "Hello, World!"), ctx);
        });

        // PostgreSQL
        app.get("/db", postgres::handleSingleDbQuery);
        app.get("/queries", postgres::handleMultipleDbQueries);
        app.get("/fortunes", postgres::handleFortunes);
        app.get("/updates", postgres::handleUpdates);

        // MongoDb
        app.get("/mongo/db", mongo::handleSingleDbQuery);
        app.get("/mongo/queries", mongo::handleMultipleDbQueries);
        app.get("/mongo/fortunes", mongo::handleFortunes);
        app.get("/mongo/updates", mongo::handleUpdates);

        app.exception(Exception.class, (exception, ctx) -> ctx.status(503).result("503 Service Unavailable"));
    }


    /**
     * This is a workaround for the {@code Server} header required by the TechEmpower Framework Benchmarks.
     * Simple server with a single HTTP/1.1 connector.
     * @return a new Javalin server with the {@code Server} header enabled.
     */
    private static Server createServer() {
        Server server = new Server(ConcurrencyUtil.INSTANCE.jettyThreadPool("JettyServerThreadPool"));
        ServerConnector connector;

        //The http configuration object
        HttpConfiguration httpConfiguration = new HttpConfiguration();
        httpConfiguration.setUriCompliance(UriCompliance.RFC3986);  // accept ambiguous values in path and let Javalin handle them

        //The factory for HTTP/1.1 connections.
        HttpConnectionFactory http11 = new HttpConnectionFactory(httpConfiguration);

        //The factory for HTTP/2 connections.
        connector = new ServerConnector(server,http11);

        connector.setPort(8080);

        server.addConnector(connector);

        return server;
    }

}
