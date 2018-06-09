package org.tio.http.server.benchmark.init;

import java.io.IOException;

import org.tio.http.common.HttpConfig;
import org.tio.http.common.handler.HttpRequestHandler;
import org.tio.http.common.session.id.ISessionIdGenerator;
import org.tio.http.common.session.id.impl.SnowflakeSessionIdGenerator;
import org.tio.http.server.HttpServerStarter;
import org.tio.http.server.benchmark.controller.TestController;
import org.tio.http.server.handler.DefaultHttpRequestHandler;
import org.tio.http.server.mvc.Routes;

/**
 * @author tanyaowu
 * 2017年7月19日 下午4:59:04
 */
public class HttpServerInit {

	public static HttpConfig httpConfig;

	public static HttpRequestHandler requestHandler;

	public static HttpServerStarter httpServerStarter;

	public static void init() throws Exception {
		httpConfig = new HttpConfig(8080, null, null, null);
		httpConfig.setUseSession(false);
		
		int workerid = 1;
		int datacenter = 1;
		ISessionIdGenerator sessionIdGenerator = new SnowflakeSessionIdGenerator(workerid, datacenter);
		httpConfig.setSessionIdGenerator(sessionIdGenerator);
		
		String[] scanPackages = new String[] { TestController.class.getPackage().getName() };
		Routes routes = new Routes(scanPackages);
		
		DefaultHttpRequestHandler requestHandler = new DefaultHttpRequestHandler(httpConfig, routes);

		httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
		httpServerStarter.start();
	}

	/**
	 * @param args
	 * @author tanyaowu
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
	}

	/**
	 *
	 * @author tanyaowu
	 */
	public HttpServerInit() {
	}
}
