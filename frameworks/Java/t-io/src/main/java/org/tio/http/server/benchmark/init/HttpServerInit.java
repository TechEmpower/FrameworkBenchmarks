package org.tio.http.server.benchmark.init;

import org.tio.http.common.HttpConfig;
import org.tio.http.common.handler.HttpRequestHandler;
import org.tio.http.server.HttpServerStarter;
import org.tio.http.server.benchmark.controller.TestController;
import org.tio.http.server.handler.DefaultHttpRequestHandler;
import org.tio.http.server.mvc.Routes;
import org.tio.server.ServerGroupContext;

/**
 * @author tanyaowu
 * 2017年7月19日 下午4:59:04
 */
public class HttpServerInit {
	public static HttpConfig httpConfig;

	public static HttpRequestHandler requestHandler;

	public static HttpServerStarter httpServerStarter;
	
	public static ServerGroupContext serverGroupContext;

	public static void init() throws Exception {
		httpConfig = new HttpConfig(8080, null, null, null);
		httpConfig.setUseSession(false);
		httpConfig.setWelcomeFile(null);
		httpConfig.setCheckHost(false);
		httpConfig.setCompatible1_0(false);

		String[] scanPackages = new String[] { TestController.class.getPackage().getName() };
		Routes routes = new Routes(scanPackages);

		requestHandler = new DefaultHttpRequestHandler(httpConfig, routes);
		httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
		serverGroupContext = httpServerStarter.getServerGroupContext();
		serverGroupContext.statOn = false;
		httpServerStarter.start();
	}
}
