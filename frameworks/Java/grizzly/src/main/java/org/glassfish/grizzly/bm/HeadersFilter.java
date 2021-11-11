package org.glassfish.grizzly.bm;

import java.io.IOException;

import org.glassfish.grizzly.filterchain.FilterChainContext;
import org.glassfish.grizzly.filterchain.NextAction;
import org.glassfish.grizzly.http.HttpBaseFilter;
import org.glassfish.grizzly.http.HttpContent;
import org.glassfish.grizzly.http.HttpPacket;
import org.glassfish.grizzly.http.HttpRequestPacket;
import org.glassfish.grizzly.http.HttpResponsePacket;
import org.glassfish.grizzly.http.util.FastHttpDateFormat;
import org.glassfish.grizzly.http.util.Header;

/**
 * Must be added just before the HttpServerFilter i.e. second to last in the filter chain.
 * 
 * @author zloster
 *
 */
public class HeadersFilter extends HttpBaseFilter {

	@Override
	protected void bind(HttpRequestPacket request, HttpResponsePacket response) {
		// This is never called. I don't know why.
		super.bind(request, response);
	}

	@Override
	public NextAction handleRead(FilterChainContext ctx) throws IOException {
		// Taken from HttpServerFilter
		final Object message = ctx.getMessage();
		if (HttpPacket.isHttp(message)) {
			// Otherwise cast message to a HttpContent
			final HttpContent httpContent = (HttpContent) message;
			final HttpRequestPacket request = (HttpRequestPacket) httpContent.getHttpHeader();
			final HttpResponsePacket response = request.getResponse();
			response.setHeader(Header.Server, Server.SERVER_VERSION);
			response.setHeader(Header.Date, FastHttpDateFormat.getCurrentDate());
		}
		return super.handleRead(ctx);
	}

}