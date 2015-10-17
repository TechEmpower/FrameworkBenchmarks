package vertx;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.json.Json;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.Collections;

public class WebServer extends AbstractVerticle implements Handler<HttpServerRequest> {

	static Logger logger = LoggerFactory.getLogger(WebServer.class.getName());

	private static final String PATH_PLAINTEXT = "/plaintext";
	private static final String PATH_JSON = "/json";

	private static final String RESPONSE_TYPE_PLAIN = "text/plain";
	private static final String RESPONSE_TYPE_JSON = "application/json";

	private static final String TEXT_MESSAGE = "message";
	private static final String HELLO_WORLD = "Hello, world!";
	private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD);

	private static final String HEADER_SERVER = "SERVER";
	private static final String HEADER_DATE = "DATE";
	private static final String HEADER_CONTENT = "content-type";

	private static final String SERVER = "vertx";

	private String dateString;

	private HttpServer server;

	@Override
	public void start() {

		int port = 8080;

		server = vertx.createHttpServer();

		server.requestHandler(WebServer.this).listen(port);

		dateString = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());

		vertx.setPeriodic(1000, handler -> {
			dateString = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());
		});
	}

	@Override
	public void handle(HttpServerRequest request) {
		switch (request.path()) {
		case PATH_PLAINTEXT:
			handlePlainText(request);
			break;
		case PATH_JSON:
			handleJson(request);
			break;
		default:
			request.response().setStatusCode(404);
			request.response().end();
		}
	}

	@Override
	public void stop(){
		if ( server != null ) server.close();        
	}

	private void handlePlainText(HttpServerRequest request) {
		request.response()
		.putHeader(HEADER_CONTENT, RESPONSE_TYPE_PLAIN).putHeader(HEADER_SERVER,  SERVER)
		.putHeader(HEADER_DATE, dateString).end(HELLO_WORLD_BUFFER);
	}

	private void handleJson(HttpServerRequest request) {
		Buffer buff = Buffer.buffer(Json.encode(Collections.singletonMap(TEXT_MESSAGE, HELLO_WORLD)));
		request.response().putHeader(HEADER_CONTENT, RESPONSE_TYPE_JSON).putHeader(HEADER_SERVER,  SERVER)
		.putHeader(HEADER_DATE, dateString).end(buff);
	}
	
	public static void main(String[] args) {
		int procs = Runtime.getRuntime().availableProcessors();
		Vertx vertx = Vertx.vertx();
		vertx.deployVerticle(WebServer.class.getName(), 
				new DeploymentOptions().setInstances(procs*2), event -> {
					if (event.succeeded()) {
						logger.debug("Your Vert.x application is started!");
					} else {
						logger.error("Unable to start your application", event.cause());
					}
				});
	}
}
