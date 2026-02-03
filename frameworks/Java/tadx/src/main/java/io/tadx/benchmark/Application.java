package io.tadx.benchmark;

/*import io.netty.util.concurrent.MultithreadEventExecutorGroup;
import io.tadx.core.logging.LogFactory;
import io.vertx.core.*;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.*;
import io.vertx.core.impl.SysProps;
import io.vertx.core.internal.VertxInternal;
import io.vertx.core.json.JsonObject;*/
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/*import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;*/


/**
 * The main program loads the configuration and starts the server by `tadx-web` <br/>
 *
 * @author Tad.x team
 * @since 1.0.0
 */
@SpringBootApplication
public class Application { // extends VerticleBase implements Handler<HttpServerRequest>

    public static void main(String[] args) {

		SpringApplication.run(Application.class, args);


        /*Vertx vertx = Vertx.vertx(new VertxOptions()
                .setEventLoopPoolSize(Runtime.getRuntime().availableProcessors())
                .setPreferNativeTransport(true)
                .setDisableTCCL(true)
        );
        vertx.exceptionHandler(err -> {
            LogFactory.frameworkLogger().warn("Uncaught exception: " + err);
        });
        vertx.deployVerticle(Application.class.getName(), new DeploymentOptions().setInstances(Runtime.getRuntime().availableProcessors()))
                .onComplete(event -> {
                    if (event.succeeded()) {
                        LogFactory.frameworkLogger().warn("Server listening on port " + 8080);
                    } else {
                        LogFactory.frameworkLogger().warn("Unable to start your application "+event.cause());
                    }
                });*/
	}

    /*

    private HttpServer server;
    private CharSequence dateString;
    private MultiMap plaintextHeaders;
    private MultiMap jsonHeaders;private static final String PATH_PLAINTEXT = "/plaintext";
    private static final String PATH_JSON = "/json";
    private static final String PATH_DB = "/db";
    private static final String PATH_QUERIES = "/queries";
    private static final String PATH_UPDATES = "/updates";
    private static final String PATH_FORTUNES = "/fortunes";
    private static final String PATH_CACHING = "/cached-queries";

    private static final CharSequence RESPONSE_TYPE_PLAIN = HttpHeaders.createOptimized("text/plain");
    private static final CharSequence RESPONSE_TYPE_HTML = HttpHeaders.createOptimized("text/html; charset=UTF-8");
    private static final CharSequence RESPONSE_TYPE_JSON = HttpHeaders.createOptimized("application/json");

    private static final String HELLO_WORLD = "Hello, world!";
    private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8");

    private static final CharSequence HEADER_SERVER = HttpHeaders.SERVER;
    private static final CharSequence HEADER_DATE = HttpHeaders.DATE;
    private static final CharSequence HEADER_CONTENT_TYPE = HttpHeaders.CONTENT_TYPE;
    private static final CharSequence HEADER_CONTENT_LENGTH = HttpHeaders.CONTENT_LENGTH;

    private static final CharSequence HELLO_WORLD_LENGTH = HttpHeaders.createOptimized("" + HELLO_WORLD.length());
    private static final CharSequence JSON_LENGTH = HttpHeaders.createOptimized("" + JsonObject.of("message", "Hello, World!").toString().length());
    private static final CharSequence SERVER = HttpHeaders.createOptimized("vert.x");
    @Override
    public Future<?> start() throws Exception {
        int port = 8000;
        printConfig((VertxInternal) vertx);
        server = vertx.createHttpServer(new HttpServerOptions()
                        .setHttp2ClearTextEnabled(false)
                        .setStrictThreadMode(true))
                .requestHandler(Application.this);
        dateString = createDateHeader();
        plaintextHeaders = plaintextHeaders();
        jsonHeaders = jsonHeaders();
        JsonObject config = config();
        vertx.setPeriodic(1000, id -> {
            dateString = createDateHeader();
            plaintextHeaders = plaintextHeaders();
            jsonHeaders = jsonHeaders();
        });
        return server.listen(port);
    }

    @Override
    public Future<?> stop() throws Exception {
        return super.stop();
    }

    @Override
    public void handle(HttpServerRequest request) {
        try {
            switch (request.path()) {
                case PATH_PLAINTEXT:
                    handlePlainText(request);
                    break;
                case PATH_JSON:
                    handleJson(request);
                    break;
                default:
                    request.response()
                            .setStatusCode(404)
                            .end();
                    break;
            }
        } catch (Exception e) {
            LogFactory.frameworkLogger().warn("Error: " + e);
            request.response().setStatusCode(500).end();
        }
    }

    private void handlePlainText(HttpServerRequest request) {
        HttpServerResponse response = request.response();
        response.headers().setAll(plaintextHeaders);
        response.end(HELLO_WORLD_BUFFER);
    }

    private void handleJson(HttpServerRequest request) {
        HttpServerResponse response = request.response();
        response.headers().setAll(jsonHeaders);
        response.end(JsonObject.of("message", "Hello, World!").toString());
    }

    public static CharSequence createDateHeader() {
        return HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    private MultiMap plaintextHeaders() {
        return HttpHeaders
                .headers()
                .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_PLAIN)
                .add(HEADER_SERVER, SERVER)
                .add(HEADER_DATE, dateString)
                .add(HEADER_CONTENT_LENGTH, HELLO_WORLD_LENGTH)
                .copy(false);
    }

    private MultiMap jsonHeaders() {
        return HttpHeaders
                .headers()
                .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
                .add(HEADER_SERVER, SERVER)
                .add(HEADER_DATE, dateString)
                .add(HEADER_CONTENT_LENGTH, JSON_LENGTH)
                .copy(false);
    }

    private static void printConfig(VertxInternal vertx) {
        boolean nativeTransport = vertx.isNativeTransportEnabled();
        String transport = vertx.transport().getClass().getSimpleName();
        String version = "unknown";
        try {
            InputStream in = Vertx.class.getClassLoader().getResourceAsStream("META-INF/vertx/vertx-version.txt");
            if (in == null) {
                in = Vertx.class.getClassLoader().getResourceAsStream("vertx-version.txt");
            }
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buffer = new byte[256];
            while (true) {
                int amount = in.read(buffer);
                if (amount == -1) {
                    break;
                }
                out.write(buffer, 0, amount);
            }
            version = out.toString();
        } catch (IOException e) {
            LogFactory.frameworkLogger().warn("Could not read Vertx version" + e);;
        }
        LogFactory.frameworkLogger().warn("Vertx: " + version);
        LogFactory.frameworkLogger().warn("Processors: " + Runtime.getRuntime().availableProcessors());
        LogFactory.frameworkLogger().warn("Event Loop Size: " + ((MultithreadEventExecutorGroup)vertx.nettyEventLoopGroup()).executorCount());
        LogFactory.frameworkLogger().warn("Native transport : " + nativeTransport);
        LogFactory.frameworkLogger().warn("Transport : " + transport);
        LogFactory.frameworkLogger().warn("Netty buffer bound check : " + System.getProperty("io.netty.buffer.checkBounds"));
        LogFactory.frameworkLogger().warn("Netty buffer accessibility check : " + System.getProperty("io.netty.buffer.checkAccessible"));
        for (SysProps sysProp : SysProps.values()) {
            LogFactory.frameworkLogger().warn(sysProp.name +  " : " + sysProp.get());
        }
    }*/
}
