package benchmark;

import benchmark.repository.DbFactory;
import gg.jte.ContentType;
import gg.jte.TemplateEngine;
import io.javalin.Javalin;
import io.javalin.rendering.template.JavalinJte;
import java.util.Collections;
import java.util.List;
import io.javalin.http.servlet.DefaultTasks;

public class Main {

    public static void main(String[] args) {

        TemplateEngine templateEngine = TemplateEngine.createPrecompiled(ContentType.Html);
        templateEngine.setTrimControlStructures(true);
        JavalinJte.init(templateEngine, c -> false);

        DslJsonMapper jsonMapper = new DslJsonMapper();

        DatabaseController postgres = new DatabaseController(jsonMapper, DbFactory.INSTANCE.getDbService(DbFactory.DbType.POSTGRES));
        DatabaseController mongo = new DatabaseController(jsonMapper, DbFactory.INSTANCE.getDbService(DbFactory.DbType.MONGODB));

        Javalin app = Javalin.create(config -> {
            config.compression.none();
            config.jetty.server(ServerUtil::createServer);
            config.pvt.servletRequestLifecycle = List.of(DefaultTasks.INSTANCE.getHTTP());
        }).start(8080);

        app.get("/plaintext", ctx -> ctx.result("Hello, World!"));
        app.get("/json", ctx -> jsonMapper.writeJson(Collections.singletonMap("message", "Hello, World!"), ctx));

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

}
