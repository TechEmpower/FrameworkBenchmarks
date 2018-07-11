package org.tio.http.server.benchmark.controller;

import org.tio.http.common.HttpRequest;
import org.tio.http.common.HttpResponse;
import org.tio.http.common.MimeType;
import org.tio.http.server.annotation.RequestPath;
import org.tio.http.server.benchmark.model.Message;
import org.tio.http.server.util.Resps;

/**
 * ab -k -n1000000 -c10 http://127.0.0.1:8080/json
 * ab -k -n1000000 -c10 http://127.0.0.1:8080/plaintext
 * @author tanyaowu
 *
 */
@RequestPath(value = "/")
public class TestController {

	private static final String HELLO_WORLD = "Hello, World!";
	
	private static final byte[] HELLO_WORLD_BYTES = HELLO_WORLD.getBytes();
	
	@RequestPath(value = "json")
	public HttpResponse json(HttpRequest request) throws Exception {
		request.channelContext.groupContext.setUseQueueSend(false);
		return Resps.json(request, new Message(HELLO_WORLD));
	}

	@RequestPath(value = "plaintext")
	public HttpResponse plaintext(HttpRequest request) throws Exception {
		request.channelContext.groupContext.setUseQueueSend(true);
		return Resps.bytesWithContentType(request, HELLO_WORLD_BYTES, MimeType.TEXT_PLAIN_TXT.getType());
	}
}
