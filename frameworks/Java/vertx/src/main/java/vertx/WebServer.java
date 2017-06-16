package vertx;

import io.netty.util.AsciiString;
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

import com.sun.corba.se.impl.ior.ByteBuffer;

import static io.vertx.core.http.HttpHeaders.*;

public class WebServer extends AbstractVerticle implements Handler<HttpServerRequest> {

	static Logger logger = LoggerFactory.getLogger(WebServer.class.getName());

	private static final String PATH_PLAINTEXT = "/plaintext";
	private static final String PATH_JSON = "/json";

	private static final CharSequence RESPONSE_TYPE_PLAIN = new AsciiString("text/plain");
	private static final CharSequence RESPONSE_TYPE_JSON = new AsciiString("application/json");

	private static final CharSequence TEXT_MESSAGE = new AsciiString("message");
	private static final byte[] HELLO_WORLD = "Hello, world!".getBytes();
	private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD);
	private static final CharSequence HELLO_WORLD_CONTENT_LENGTH = new AsciiString(String.valueOf(HELLO_WORLD.length));

	private static final CharSequence VERTX = new AsciiString("vertx".toCharArray());

	private CharSequence dateString;

	private HttpServer server;

	@Override
	public void start() {

		int port = 8080;

		server = vertx.createHttpServer();

		server.requestHandler(WebServer.this).listen(port);

		dateString = new AsciiString(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now()).getBytes());

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
		.putHeader(CONTENT_TYPE , RESPONSE_TYPE_PLAIN).putHeader(SERVER, VERTX)
		.putHeader(DATE, dateString).putHeader(CONTENT_TYPE, HELLO_WORLD_CONTENT_LENGTH).end(HELLO_WORLD_BUFFER);
	}

	private void handleJson(HttpServerRequest request) {
		Buffer buff = Buffer.buffer(Json.encode(Collections.singletonMap(TEXT_MESSAGE, HELLO_WORLD)));
		request.response().putHeader(CONTENT_TYPE, RESPONSE_TYPE_JSON).putHeader(SERVER,  VERTX)
		.putHeader(DATE, dateString).end(buff);
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
