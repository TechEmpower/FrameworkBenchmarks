package hello.handler;

import org.eclipse.jetty.server.*;
import org.eclipse.jetty.util.Callback;


/**
 * An implementation of the TechEmpower benchmark tests using the Jetty web
 * server.
 */
public final class HelloWebServer {
    public static void main(String[] args) throws Exception {
        Server server = new Server(8080);
        ServerConnector connector = server.getBean(ServerConnector.class);
        HttpConfiguration config = connector.getBean(HttpConnectionFactory.class).getHttpConfiguration();
        config.setSendDateHeader(true);
        config.setSendServerVersion(true);

        PathHandler pathHandler = new PathHandler();
        server.setHandler(pathHandler);

        server.start();
        server.join();
    }

    public static class PathHandler extends Handler.Abstract {
        JsonHandler _jsonHandler = new JsonHandler();
        PlainTextHandler _plainHandler = new PlainTextHandler();

        public PathHandler() {
            addBean(_jsonHandler);
            addBean(_plainHandler);
        }

        @Override
        public void setServer(Server server) {
            super.setServer(server);
            _jsonHandler.setServer(server);
            _plainHandler.setServer(server);
        }

        @Override
        public boolean handle(Request request, Response response, Callback callback) {
            String uri = request.getHttpURI().getPath();

            if ("/plaintext".equals(uri))
                return _plainHandler.handle(request, response, callback);
            else if ("/json".equals(uri))
                return _jsonHandler.handle(request, response, callback);
            else
                return false;
        }
    }
}
