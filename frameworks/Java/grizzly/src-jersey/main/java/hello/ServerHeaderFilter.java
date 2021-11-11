package hello;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.MultivaluedMap;

import org.glassfish.grizzly.http.util.FastHttpDateFormat;
import org.glassfish.grizzly.http.util.Header;

public class ServerHeaderFilter implements ContainerResponseFilter {

	@Override
	public void filter(ContainerRequestContext request, ContainerResponseContext response)
			throws IOException {
		MultivaluedMap<String, Object> headers = response.getHeaders();
		headers.putSingle(Header.Server.toString(), "GRZLY"); // The same as raw
																// Grizzly test
																// implementation
		headers.putSingle(Header.Date.toString(), FastHttpDateFormat.getCurrentDate());
	}
}