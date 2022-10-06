package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.DbFactory;
import benchmark.repository.DbService;
import com.mitchellbosecke.pebble.PebbleEngine;
import com.mitchellbosecke.pebble.loader.ClasspathLoader;
import io.javalin.Javalin;
import io.javalin.http.Context;

import io.javalin.rendering.template.JavalinPebble;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class Main {

    public static final int MIN_QUERIES = 1;
    public static final int MAX_QUERIES = 500;
    public static final int SERVICE_UNAVAILABLE_CODE = 503;
    public static final String SERVICE_UNAVAILABLE_TEXT = "503 Service Unavailable";

    public static void main(String[] args) {

        JavalinPebble.init(new PebbleEngine.Builder()
            .loader(new ClasspathLoader())
            .strictVariables(false)
            .build());

        Javalin app = Javalin
                .create(config -> config.compression.none())
                .start(8080);

        app.get("/plaintext", Main::handlePlainText);
        app.get("/json", Main::handleJson);

        // PostgreSQL
        app.get("/db", Main::handleSingleDbQuery);
        app.get("/queries", Main::handleMultipleDbQueries);
        app.get("/fortunes", Main::handleFortunes);
        app.get("/updates", Main::handleUpdates);

        // MongoDb
        app.get("/mongo/db", Main::handleSingleDbQuery);
        app.get("/mongo/queries", Main::handleMultipleDbQueries);
        app.get("/mongo/fortunes", Main::handleFortunes);
        app.get("/mongo/updates", Main::handleUpdates);
    }


    private static void handlePlainText(Context ctx) {
        ctx.result("Hello, World!");
    }

    private static void handleJson(Context ctx) {
        ctx.json(Collections.singletonMap("message", "Hello, World!"));
    }

    private static void handleSingleDbQuery(Context ctx) {

        DbService dbService = getDbServiceFromPath(ctx.path());

        try {
            ctx.json(dbService.getWorld(1).get(0));
        } catch (Throwable t) {
            ctx.status(SERVICE_UNAVAILABLE_CODE).result(SERVICE_UNAVAILABLE_TEXT);
        }
    }

    private static void handleMultipleDbQueries(Context ctx) {

        int num = getBoundedRowNumber(ctx.queryParam("queries"));
        DbService dbService = getDbServiceFromPath(ctx.path());

        try {
            ctx.json(dbService.getWorld(num));
        } catch (Throwable t) {
            ctx.status(SERVICE_UNAVAILABLE_CODE).result(SERVICE_UNAVAILABLE_TEXT);
        }
    }

    private static void handleFortunes(Context ctx) {

        DbService dbService = getDbServiceFromPath(ctx.path());

        try {
            List<Fortune> fortuneList = dbService.getFortune();
            Map<String, List<Fortune>> map = Collections.singletonMap("list", fortuneList);
            ctx.render("fortune.pebble", map).header("Content-Type", "text/html; charset=utf-8");
        } catch (Throwable t) {
            ctx.status(SERVICE_UNAVAILABLE_CODE).result(SERVICE_UNAVAILABLE_TEXT);
        }
    }

    private static void handleUpdates(Context ctx) {

        int num = getBoundedRowNumber(ctx.queryParam("queries"));
        DbService dbService = getDbServiceFromPath(ctx.path());

        try {
            ctx.json(dbService.updateWorld(num));
        } catch (Throwable t) {
            ctx.status(SERVICE_UNAVAILABLE_CODE).result(SERVICE_UNAVAILABLE_TEXT);
        }
    }

    private static DbService getDbServiceFromPath(String path) {

        return (path.contains("mongo")) ?
                DbFactory.INSTANCE.getDbService(DbFactory.DbType.MONGODB) :
                DbFactory.INSTANCE.getDbService(DbFactory.DbType.POSTGRES);
    }

    private static int getBoundedRowNumber(String number) {

        int num;
        try {
            num = Integer.parseInt(number);
        } catch (NumberFormatException e) {
            num = MIN_QUERIES;
        }
        return Math.max(MIN_QUERIES, Math.min(num, MAX_QUERIES));
    }
}
