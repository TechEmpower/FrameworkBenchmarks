package hello;
import static io.netty.handler.codec.http.HttpHeaderNames.DATE;
import static io.netty.handler.codec.http.HttpHeaderNames.SERVER;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import org.restexpress.Request;
import org.restexpress.Response;
import org.restexpress.pipeline.Postprocessor;

public class RequiredResponseHeaders implements Postprocessor {
	private static final String HEADER_SERVER = SERVER.toString();
	private static final String SERVER_NAME = "RestExpress";
	private static final String HEADER_DATE = DATE.toString();
	
	@Override
	public void process(Request request, Response response) {
		response.addHeader(HEADER_SERVER, SERVER_NAME);
		response.addHeader(HEADER_DATE, DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
	}

}
