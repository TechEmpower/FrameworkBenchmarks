package hello.servlet;

import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.servlet.FiberHttpServlet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

@SuppressWarnings("serial")
public final class PlaintextServlet extends FiberHttpServlet {
    private static final byte[] helloWorld = "Hello, World!".getBytes(StandardCharsets.ISO_8859_1);

    @Override
    @Suspendable
    protected final void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setContentType("text/plain");
        resp.setHeader("Server", "comsat-servlet-undertow");
        resp.getOutputStream().write(helloWorld);
    }
}
