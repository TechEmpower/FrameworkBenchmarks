package hello.webactors;

import co.paralleluniverse.comsat.webactors.undertow.AutoWebActorHandler;
import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import hello.Server;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;

import org.xnio.Options;

public final class UndertowServer implements Server {
    @Override
    public final void run() throws Exception {
        WebActor.SERVER = "comsat-webactors-undertow";
        //noinspection deprecation
        final Undertow u = io.undertow.Undertow.builder()
            .setHandler(new AutoWebActorHandler())

            .setIoThreads(100)
            .setWorkerThreads(100)
            .setServerOption(Options.CONNECTION_HIGH_WATER, 100_000)
            .setServerOption(Options.CONNECTION_LOW_WATER, 100_000)

            .setDirectBuffers(true)
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

        new Thread(u::start).start();

        System.err.println("Server is up.");

        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");
        System.err.println("Server test cases are instrumented and bootstrapped.");
    }
}
