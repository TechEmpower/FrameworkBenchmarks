package org.voovan;

import org.voovan.http.message.HttpStatic;
import org.voovan.http.server.HttpRequest;
import org.voovan.http.server.HttpResponse;
import org.voovan.http.server.HttpRouter;
import org.voovan.http.server.WebServer;
import org.voovan.http.server.context.WebContext;
import org.voovan.http.server.context.WebServerConfig;
import org.voovan.tools.TEnv;
import org.voovan.tools.TObject;
import org.voovan.tools.json.JSON;
import org.voovan.tools.log.Logger;

import java.util.Map;


public class VoovanTFB {
	private static final byte[] HELLO_WORLD = "Hello, World!".getBytes();


	public static void main(String[] args) {

		WebServerConfig webServerConfig = WebContext.getWebServerConfig();
		webServerConfig.setGzip(false);
		webServerConfig.setAccessLog(false);
		webServerConfig.setKeepAliveTimeout(1000);
		webServerConfig.setHost("0.0.0.0");
		webServerConfig.setPort(8080);
		webServerConfig.setHotSwapInterval(0);
		webServerConfig.setCache(true);
		webServerConfig.getModuleonfigs().clear();
		webServerConfig.getRouterConfigs().clear();
		WebServer webServer = WebServer.newInstance(webServerConfig);

		//性能测试请求;
		webServer.get("/plaintext", new HttpRouter() {
			public void process(HttpRequest req, HttpResponse resp) throws Exception {
				resp.header().put(HttpStatic.CONTENT_TYPE_STRING, "text/plain");
				resp.write(HELLO_WORLD);
			}
		});
		//性能测试请求
		webServer.get("/json", new HttpRouter() {
			public void process(HttpRequest req, HttpResponse resp) throws Exception {
				resp.header().put("Content-Type", "application/json");
				resp.write(JSON.toJSON(TObject.asMap("message", "Hello, World!")));
			}
		});


		webServer.syncServe();	
		
		TEnv.sleep(2000);
		Logger.setEnable(true);	
	}
}
