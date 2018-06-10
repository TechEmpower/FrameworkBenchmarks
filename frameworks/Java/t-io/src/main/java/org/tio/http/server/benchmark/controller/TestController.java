package org.tio.http.server.benchmark.controller;

import org.tio.http.common.HttpRequest;
import org.tio.http.common.HttpResponse;
import org.tio.http.server.annotation.RequestPath;
import org.tio.http.server.benchmark.model.Message;
import org.tio.http.server.util.Resps;

/**
 * @author tanyaowu
 * 2017年6月29日 下午7:53:59
 */
@RequestPath(value = "/")
public class TestController {

	private static final String HELLO_WORLD = "Hello, World!";

	@RequestPath(value = "json")
	public HttpResponse json(HttpRequest request) throws Exception {
		return Resps.json(request, new Message(HELLO_WORLD));
	}

	@RequestPath(value = "plaintext")
	public HttpResponse plaintext(HttpRequest request) throws Exception {
		return Resps.txt(request, HELLO_WORLD);
	}
}
