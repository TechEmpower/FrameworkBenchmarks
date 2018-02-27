package hello.servlet;

import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import hello.Server;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.DeploymentManager;

import org.xnio.Options;

import java.io.IOException;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow servlet container.
 */
public final class UndertowServer implements Server {
    @Override
    public final void run() throws Exception {
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

            .setServerOption(Options.CONNECTION_HIGH_WATER, 100_000)
            .setServerOption(Options.CONNECTION_LOW_WATER, 100_000)

            .setBufferSize(1024)
            .setBuffersPerRegion(100)

            // .setSocketOption(Options.ALLOW_BLOCKING, true)
            .setSocketOption(Options.REUSE_ADDRESSES, true)
            // .setSocketOption(Options.CORK, true)
            // .setSocketOption(Options.USE_DIRECT_BUFFERS, true)
            .setSocketOption(Options.BACKLOG, 100000)
            .setSocketOption(Options.TCP_NODELAY, true)
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
                } catch (final Exception e) {
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
