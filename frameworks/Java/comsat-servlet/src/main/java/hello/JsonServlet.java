package hello;

import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.servlet.FiberHttpServlet;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public final class JsonServlet extends FiberHttpServlet {
    private static final class HelloWorldData {
        @SuppressWarnings("unused")
        public final String message = "Hello, World!";
    }

    private static final ObjectMapper mapper = new ObjectMapper();
    private static final HelloWorldData helloWorld = new HelloWorldData();

    @Override
    @Suspendable
    protected final void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setContentType("application/json");
        resp.setHeader("Server", "comsat-servlet");
        mapper.writeValue(resp.getOutputStream(), helloWorld);
    }
}
