package org.tio.http.server.benchmark.controller;

import org.tio.http.common.HeaderName;
import org.tio.http.common.HeaderValue;
import org.tio.http.common.HttpRequest;
import org.tio.http.common.HttpResponse;
import org.tio.http.server.annotation.RequestPath;
import org.tio.http.server.benchmark.model.Message;
import org.tio.utils.json.Json;

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
		//更高性能的写法
		HttpResponse ret = new HttpResponse(request);
		ret.setBody(Json.toJson(new Message(HELLO_WORLD)).getBytes());
		ret.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
		return ret;
		
		//简便写法
//		return Resps.json(request, new Message(HELLO_WORLD));
	}

	@RequestPath(value = "plaintext")
	public HttpResponse plaintext(HttpRequest request) throws Exception {
		//更高性能的写法
		HttpResponse ret = new HttpResponse(request);
		ret.setBody(HELLO_WORLD_BYTES);
		ret.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_TXT);
		return ret;
		
		//简便写法
//		return Resps.bytesWithContentType(request, HELLO_WORLD_BYTES, MimeType.TEXT_PLAIN_TXT.getType());
	}
}
