package hello;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.server.handler.AbstractHandlerContainer;


/**
 * An implementation of the TechEmpower benchmark tests using the Jetty web
 * server.  
 */
public final class HelloWebServer 
{
    public static void main(String[] args) throws Exception
    {
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
    
    public static class PathHandler extends AbstractHandler
    {
        JsonHandler _jsonHandler=new JsonHandler();
        PlainTextHandler _plainHandler=new PlainTextHandler();
        
        public PathHandler()
        {
            addBean(_jsonHandler);
            addBean(_plainHandler);
        }

        @Override
        public void setServer(Server server)
        {
            super.setServer(server);
            _jsonHandler.setServer(server);
            _plainHandler.setServer(server);
        }

        @Override
        public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
        {
            if ("/plaintext".equals(target))
                _plainHandler.handle(target,baseRequest,request,response);
            else if ("/json".equals(target))
                _jsonHandler.handle(target,baseRequest,request,response);
        }
        
    }
}
