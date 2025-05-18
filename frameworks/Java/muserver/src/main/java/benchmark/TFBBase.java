package benchmark;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.DbFactory;
import benchmark.repository.DbService;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jakarta.rs.json.JacksonJsonProvider;
import io.muserver.Method;
import io.muserver.MuResponse;
import io.muserver.MuServerBuilder;
import io.muserver.Mutils;
import io.muserver.rest.RestHandlerBuilder;
import io.pebbletemplates.pebble.PebbleEngine;
import io.pebbletemplates.pebble.template.PebbleTemplate;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static jakarta.ws.rs.core.MediaType.*;

public class TFBBase {
    public static final Logger LOGGER = LoggerFactory.getLogger(TFBBase.class);
    public static PebbleEngine engine = new PebbleEngine.Builder().build();
    public static PebbleTemplate compiledTemplate = engine.getTemplate("templates/fortune.peb");

    public static MuServerBuilder commonBuilderWithMuHandler() {
        var jackson = new ObjectMapper();
        var helloWorld = Map.of("message", "Hello, World!");
        var dbService = DbFactory.INSTANCE.getDbService(DbFactory.DbType.POSTGRES);
        return MuServerBuilder.httpServer()
                .addHandler((ignore, res) -> {
                    res.headers().add("Server", "muserver");
                    return false;
                })
                .addHandler(Method.GET, "/plaintext", (req, res, pp) -> {
                    var ah = req.handleAsync();
                    res.headers().add("content-type", "text/plain");
                    ah.write(Mutils.toByteBuffer("Hello, World!"), (optEx) -> ah.complete());
                    // res.write("Hello, World!");
                })
                .addHandler(Method.GET, "/json", (req, res, pp) -> {
                    res.headers().add("content-type", "application/json");
                    jackson.writeValue(res.writer(), helloWorld);
                })
                .addHandler(Method.GET, "/db", (req, res, pp) -> {
                    res.headers().add("content-type", "application/json");
                    jackson.writeValue(res.writer(), dbService.getWorld(1).get(0));
                })
                .addHandler(Method.GET, "/queries", (req, res, pp) -> {
                    res.headers().add("content-type", "application/json");
                    jackson.writeValue(
                            res.writer(),
                            dbService.getWorld(getBoundedRowNumber(req.query().get("queries", "")))
                    );
                })
                .addHandler(Method.GET, "/fortunes", (req, res, pp) -> {
                    res.headers().set("content-type", "text/html;charset=utf-8");
                    compiledTemplate.evaluate(
                            res.writer(),
                            Map.of("fortunes", dbService.getFortune().stream().map(Fortune::toMap).toList()));
                })
                .addHandler(Method.GET, "/updates", (req, res, pp) -> {
                    res.headers().add("content-type", "application/json");
                    jackson.writeValue(
                            res.writer(),
                            dbService.updateWorld(getBoundedRowNumber(req.query().get("queries", "")))
                    );
                })
                .withInterface("0.0.0.0")
                .withHttpPort(8080)
                .addShutdownHook(true);
    }

    public static MuServerBuilder commonBuilderWithRestHandler() {
        return MuServerBuilder.httpServer()
                .addHandler((req, res) -> {
                    res.headers().add("Server", "muserver");
                    return false;
                })

                .addHandler(RestHandlerBuilder.restHandler(new TFBResource(
                        DbFactory.INSTANCE.getDbService(DbFactory.DbType.POSTGRES),
                        compiledTemplate
                )).addCustomWriter(new JacksonJsonProvider()))
                .withInterface("0.0.0.0")
                .withHttpPort(8080)
                .addShutdownHook(true);
    }

    @Path("/rest")
    public static class TFBResource {
        public final DbService dbService;
        public final PebbleTemplate template;
        public final Map<String, String> helloWorld = Map.of("message", "Hello, World!");

        public TFBResource(
                DbService dbService,
                PebbleTemplate template
        ) {
            this.dbService = dbService;
            this.template = template;
        }

        @GET
        @Path("/plaintext")
        @Produces(TEXT_PLAIN)
        public String plaintext() {
            return "Hello, World!";
        }

        @GET
        @Path("/json")
        @Produces(APPLICATION_JSON)
        public Map<String, String> json() {
            return helloWorld;
        }

        @GET
        @Path("/db")
        @Produces(APPLICATION_JSON)
        public World db() {
            return dbService.getWorld(1).get(0);
        }

        @GET
        @Path("/queries")
        @Produces(APPLICATION_JSON)
        public List<World> queries(
                @QueryParam("queries")
                String queries
        ) {
            int num = getBoundedRowNumber(queries);
            return dbService.getWorld(num);
        }


        @GET
        @Path("/fortunes")
        @Produces(TEXT_HTML)
        public void fortunes(
                @Context MuResponse res
        ) throws IOException {
            res.headers().set("content-type", "text/html;charset=utf-8");
            List<Fortune> fortuneList = dbService.getFortune();
            var writer = res.writer();
            // writer will be flushed
            template.evaluate(writer, Map.of("fortunes", fortuneList.stream().map(Fortune::toMap).toList()));
        }

        @GET
        @Path("/updates")
        @Produces(APPLICATION_JSON)
        public List<World> updates(
                @QueryParam("queries")
                String queries
        ) {
            return dbService.updateWorld(getBoundedRowNumber(queries));
        }
    }


    private static final int MIN_QUERIES = 1;
    private static final int MAX_QUERIES = 500;

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
