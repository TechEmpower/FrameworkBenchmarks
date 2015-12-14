package hello;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.comsat.webactors.HttpRequest;
import co.paralleluniverse.comsat.webactors.HttpResponse;
import co.paralleluniverse.comsat.webactors.WebActor;
import co.paralleluniverse.fibers.SuspendExecution;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static co.paralleluniverse.comsat.webactors.HttpResponse.error;
import static co.paralleluniverse.comsat.webactors.HttpResponse.ok;

@WebActor(httpUrlPatterns = {"/plaintext", "/json"})
public class HelloWebActor extends BasicActor<Object, Void> {
	private static final class HelloWorldData {
		@SuppressWarnings("unused")
		public final String message = "Hello, World!";
	}

	private static final ObjectMapper mapper = new ObjectMapper();

	private static final Calendar calendar = Calendar.getInstance();
	private static final SimpleDateFormat dateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
	static {
		dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
	}

	@Override
	protected Void doRun() throws InterruptedException, SuspendExecution {
		//noinspection InfiniteLoopStatement
		for (;;) {
			final Object message = receive();
			if (message instanceof HttpRequest) {
				final HttpRequest req = (HttpRequest) message;
				HttpResponse.Builder res;
				switch (req.getRequestURI()) {
					case "/plaintext":
						res = ok(self(), req, "Hello, World!").setContentType("text/plain");
						break;
					case "/json":
						try {
							res = ok(self(), req, mapper.writeValueAsString(new HelloWorldData())).setContentType("application/json");
						} catch (JsonProcessingException e) {
							throw new RuntimeException(e);
						}
						break;
					default:
						res = error(self(), req, 404, "Not found");
						break;
				}
				req.getFrom().send (
					res
						.addHeader("Server", "comsat-webactors")
						.addHeader("Date", dateFormat.format(calendar.getTime()))
						.build()
				);
			}
		}
	}
}
