package org.tio.http.server.benchmark;

import java.io.IOException;

import org.tio.http.common.HttpConfig;
import org.tio.http.server.HttpServerStarter;
import org.tio.http.server.benchmark.controller.TestController;
import org.tio.http.server.handler.DefaultHttpRequestHandler;
import org.tio.http.server.mvc.Routes;
import org.tio.server.ServerTioConfig;

/**
 * @author tanyaowu 
 * 2018年6月9日 上午10:30:45
 */
public class TioBenchmarkStarter {
	public static HttpConfig				httpConfig;
	public static DefaultHttpRequestHandler	requestHandler;
	public static HttpServerStarter			httpServerStarter;
	public static ServerTioConfig			serverTioConfig;

	/**
	 * @param args
	 * @author tanyaowu
	 * @throws IOException
	 */
	public static void main(String[] args) throws Exception {
		httpConfig = new HttpConfig(8080, null, null, null);
		httpConfig.setUseSession(false);
		httpConfig.setWelcomeFile(null);
		httpConfig.setCheckHost(false);
		httpConfig.setCompatible1_0(false);

		Routes routes = new Routes(TestController.class);

		requestHandler = new DefaultHttpRequestHandler(httpConfig, routes);
		requestHandler.setCompatibilityAssignment(false);
		httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
		serverTioConfig = httpServerStarter.getServerTioConfig();
		serverTioConfig.statOn = false;
		httpServerStarter.start();
	}
}
