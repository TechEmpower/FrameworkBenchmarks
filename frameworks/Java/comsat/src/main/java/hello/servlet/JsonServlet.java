package hello.servlet;

import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.servlet.FiberHttpServlet;

import com.fasterxml.jackson.databind.ObjectMapper;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;

@SuppressWarnings("serial")
public final class JsonServlet extends FiberHttpServlet {
    private static final class HelloWorldData {
        @SuppressWarnings("unused")
        public final String message = "Hello, World!";
    }

    private static final ObjectMapper mapper = new ObjectMapper();

    @Override
    @Suspendable
    protected final void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setContentType("application/json");
        resp.setHeader("Server", "comsat-servlet-undertow");
        mapper.writeValue(resp.getOutputStream(), new HelloWorldData());
    }
}
