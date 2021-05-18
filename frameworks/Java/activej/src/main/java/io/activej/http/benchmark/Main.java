package io.activej.http.benchmark;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.runtime.Settings;
import io.activej.async.service.EventloopTaskScheduler;
import io.activej.bytebuf.ByteBuf;
import io.activej.config.Config;
import io.activej.eventloop.Eventloop;
import io.activej.http.*;
import io.activej.inject.annotation.Eager;
import io.activej.inject.annotation.Named;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.launchers.http.MultithreadedHttpServerLauncher;
import io.activej.promise.Promise;
import io.activej.worker.annotation.Worker;

import java.net.InetSocketAddress;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.atomic.AtomicReference;

import static io.activej.bytebuf.ByteBufStrings.encodeAscii;
import static io.activej.config.Config.ofClassPathProperties;
import static io.activej.config.Config.ofSystemProperties;
import static io.activej.config.converter.ConfigConverters.ofInetSocketAddress;
import static java.nio.charset.StandardCharsets.UTF_8;

public final class Main extends MultithreadedHttpServerLauncher {
	private static final HttpHeaderValue SERVER_HEADER_VALUE = HttpHeaderValue.ofBytes(encodeAscii("X"));
	private static final HttpHeaderValue JSON_CONTENT_TYPE_HEADER_VALUE = HttpHeaderValue.ofBytes(encodeAscii("application/json"));
	private static final HttpHeaderValue TEXT_CONTENT_TYPE_HEADER_VALUE = HttpHeaderValue.ofBytes(encodeAscii("text/plain"));

	private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss z");
	private static final AtomicReference<HttpHeaderValue> dateRef = new AtomicReference<>(HttpHeaderValue.ofBytes(encodeAscii(getServerTime())));

	private static final DslJson<Object> DSL_JSON = new DslJson<>(Settings.basicSetup());
	private static final byte[] PLAINTEXT_BYTES = "Hello, World!".getBytes(UTF_8);

	@Provides
	@Eager
	EventloopTaskScheduler scheduler(Eventloop eventloop) {
		return EventloopTaskScheduler.create(eventloop,
				() -> {
					dateRef.set(HttpHeaderValue.ofBytes(encodeAscii(getServerTime())));
					return Promise.complete();
				})
				.withInterval(Duration.ofSeconds(1));
	}

	@Provides
	@Worker
	AsyncServlet mainServlet(@Named("json") AsyncServlet jsonServlet, @Named("plaintext") AsyncServlet plaintextServlet) {
		return request -> {
			String path = request.getPath();
			if ("/json".equals(path)) {
				return jsonServlet.serve(request);
			} else if ("/plaintext".equals(path)) {
				return plaintextServlet.serve(request);
			} else {
				return Promise.ofException(HttpError.ofCode(400));
			}
		};
	}

	@Provides
	@Worker
	JsonWriter jsonWriter() {
		return DSL_JSON.newWriter();
	}

	@Provides
	@Worker
	@Named("json")
	AsyncServlet jsonServlet(JsonWriter writer) {
		return request -> {
			try {
				writer.reset();
				DSL_JSON.serialize(writer, new HelloWorldObject("Hello, world!"));
			} catch (Exception e) {
				return Promise.ofException(HttpError.ofCode(400, "Failed to serialize JSON", e));
			}
			return HttpResponse.ok200()
					.withBody(ByteBuf.wrap(writer.getByteBuffer(), 0, writer.size()))
					.withHeader(HttpHeaders.CONTENT_TYPE, JSON_CONTENT_TYPE_HEADER_VALUE)
					.withHeader(HttpHeaders.SERVER, SERVER_HEADER_VALUE)
					.withHeader(HttpHeaders.DATE, dateRef.get());
		};
	}

	@Provides
	@Worker
	@Named("plaintext")
	AsyncServlet plaintextServlet() {
		return request -> HttpResponse.ok200()
				.withBody(ByteBuf.wrap(PLAINTEXT_BYTES, 0, 13))
				.withHeader(HttpHeaders.CONTENT_TYPE, TEXT_CONTENT_TYPE_HEADER_VALUE)
				.withHeader(HttpHeaders.SERVER, SERVER_HEADER_VALUE)
				.withHeader(HttpHeaders.DATE, dateRef.get());
	}

	static String getServerTime() {
		return ZonedDateTime
				.now()
				.format(FORMATTER);
	}

	@Override
	protected Module getOverrideModule() {
		return new AbstractModule() {
			@Provides
			Config config() {
				return Config.create()
						.with("http.listenAddresses", Config.ofValue(ofInetSocketAddress(), new InetSocketAddress(PORT)))
						.with("workers", "" + Integer.toString(2 * Runtime.getRuntime().availableProcessors()))
						.overrideWith(ofClassPathProperties(PROPERTIES_FILE, true))
						.overrideWith(ofSystemProperties("config"));
			}
		};
	}

	public static void main(String[] args) throws Exception {
		new Main().launch(args);
	}
}
