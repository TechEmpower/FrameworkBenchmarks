package hello;

import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.servlet.FiberHttpServlet;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;
import org.xnio.Options;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow servlet container.
 */
public final class HelloWebServer {
    private static final class JsonServlet extends FiberHttpServlet {
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

    private static final class PlaintextServlet extends FiberHttpServlet {
        private static final byte[] helloWorld = "Hello, World!".getBytes(StandardCharsets.ISO_8859_1);

        @Override
        @Suspendable
        protected final void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
            resp.setContentType("text/plain");
            resp.setHeader("Server", "comsat-servlet-undertow");
            resp.getOutputStream().write(helloWorld);
        }
    }

    public static void main(String[] args) throws Exception {
        final DeploymentInfo deployment = Servlets.deployment().setDeploymentName("")
            .setClassLoader(ClassLoader.getSystemClassLoader())
            .setContextPath("/");

        deployment.addServlet(Servlets.servlet("plaintext", PlaintextServlet.class).addMapping("/plaintext").setAsyncSupported(true));
        deployment.addServlet(Servlets.servlet("json", JsonServlet.class).addMapping("/json").setAsyncSupported(true));

        final DeploymentManager servletsContainer = Servlets.defaultContainer().addDeployment(deployment);

        servletsContainer.deploy();

        final HttpHandler handler = servletsContainer.start();

        //noinspection deprecation
        final Undertow server = Undertow.builder()
            .setHandler(handler)
            .setDirectBuffers(true)

            .setIoThreads(100)
            .setWorkerThreads(100)

            .setBufferSize(1024)
            .setBuffersPerRegion(100)

            // .setSocketOption(Options.ALLOW_BLOCKING, true)
            .setSocketOption(Options.REUSE_ADDRESSES, true)
            // .setSocketOption(Options.CORK, true)
            // .setSocketOption(Options.USE_DIRECT_BUFFERS, true)
            // .setSocketOption(Options.BACKLOG, Integer.MAX_VALUE)
            // .setSocketOption(Options.RECEIVE_BUFFER, 2048)
            // .setSocketOption(Options.SEND_BUFFER, 2048)
            // .setSocketOption(Options.CONNECTION_HIGH_WATER, Integer.MAX_VALUE)
            // .setSocketOption(Options.CONNECTION_LOW_WATER, Integer.MAX_VALUE)
            // .setSocketOption(Options.READ_TIMEOUT, Integer.MAX_VALUE)
            // .setSocketOption(Options.WRITE_TIMEOUT, Integer.MAX_VALUE)
            // .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false) //don't send a keep-alive header for HTTP/1.1 requests, as it is not required

            // .setServerOption(UndertowOptions.ALWAYS_SET_DATE, true)
            .setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, false)
            .setServerOption(UndertowOptions.RECORD_REQUEST_START_TIME, false)

            .addHttpListener(8080, "0.0.0.0")
            .build();

        server.start();

        System.err.println("Server is up.");

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public final void run() {
                try {
                    server.stop();

                    System.err.println("Server is down.");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        });

        new Thread() {
            @Override
            public final void run() {
                try {
                    AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
                    AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");

                    System.err.println("Server test cases are instrumented and bootstrapped.");
                } catch (final InterruptedException | IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }.start();
    }
}
