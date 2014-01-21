package hello.web;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import spark.Request;
import spark.Response;
import spark.Route;

public abstract class LoggingRoute extends Route {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingRoute.class);
    
    public LoggingRoute(final String path) {
        super(path);
    }
    
    @Override
    public Object handle(final Request request, final Response response) {
        try {
            return handleInternal(request, response);
        } catch (RuntimeException ex) {
            LOGGER.error("Request handling failed", ex);
            throw ex;
        }
    }
    
    protected abstract Object handleInternal(Request request, Response response);
}
