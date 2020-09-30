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
import org.voovan.tools.TString;
import org.voovan.tools.reflect.TReflect;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;


public class VoovanTFB {
        private static final String HELLO_WORLD_STR = "Hello, World!";
        private static final byte[] HELLO_WORLD_BYTES = "Hello, World!".getBytes();

        public static class Message {
                private final String message;

                public Message(String message) {
                        this.message = message;
                }

                public String getMessage() {
                        return message;
                }
                
                public int hashCode(){
                     return 98821452;
                }
        }
        
        public static void main(String[] args) {
                TReflect.register(Message.class);

                WebServerConfig webServerConfig = WebContext.buildWebServerConfig();
                webServerConfig.setGzip(false);
                webServerConfig.setAccessLog(false);
                webServerConfig.setKeepAliveTimeout(1000);
                webServerConfig.setHost("0.0.0.0");
                webServerConfig.setPort(8080);
                webServerConfig.setHotSwapInterval(0);
                webServerConfig.setCache(true);
                webServerConfig.getModuleConfigs().clear();
                webServerConfig.getRouterConfigs().clear();
                webServerConfig.setEnablePathVariables(false);
                webServerConfig.setEnableWebSocket(false);
                WebServer webServer = WebServer.newInstance(webServerConfig);

                //性能测试请求;
                webServer.get("/plaintext", new HttpRouter() {
                        public void process(HttpRequest req, HttpResponse resp) throws Exception {
                                resp.header().put(HttpStatic.CONTENT_TYPE_STRING, HttpStatic.TEXT_PLAIN_STRING);
                                resp.write(HELLO_WORLD_BYTES);
                        }
                });
                //性能测试请求
                webServer.get("/json", new HttpRouter() {
                        public void process(HttpRequest req, HttpResponse resp) throws Exception {
                                resp.header().put(HttpStatic.CONTENT_TYPE_STRING, HttpStatic.APPLICATION_JSON_STRING);
                                resp.write(TString.toAsciiBytes(JSON.toJSON(new Message(HELLO_WORLD_STR), false, false)));
                        }
                });

                Logger.setEnable(false);

                webServer.serve();
        }
}
