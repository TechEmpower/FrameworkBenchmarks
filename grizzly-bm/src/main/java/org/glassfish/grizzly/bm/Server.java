package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.filterchain.FilterChainBuilder;
import org.glassfish.grizzly.http.server.AddOn;
import org.glassfish.grizzly.http.server.FileCacheFilter;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.util.HttpPipelineOptAddOn;
import org.glassfish.grizzly.http.util.HeaderValue;
import org.glassfish.grizzly.memory.PooledMemoryManager;
import org.glassfish.grizzly.nio.transport.TCPNIOTransport;
import org.glassfish.grizzly.utils.IdleTimeoutFilter;

/**
 * HttpServer
 */
public class Server {
    public static final HeaderValue SERVER_VERSION =
            HeaderValue.newHeaderValue("GRZLY").prepare();
    
    // The RequestExecutorProvider, which will run HTTP request processing
    // in the same thread
    static final RequestExecutorProvider EXECUTOR_PROVIDER =
            new RequestExecutorProvider.SameThreadProvider();
    
    public static void main(String[] args) throws Exception {
        final int port = args.length > 0
                ? Integer.parseInt(args[0]) : 8080;
        
        final HttpServer httpServer = new HttpServer();
        final NetworkListener networkListener = new NetworkListener(
                "http-listener", "0.0.0.0", port);
        final TCPNIOTransport transport = networkListener.getTransport();
        
        // force to not initialize worker thread pool
        transport.setWorkerThreadPoolConfig(null);
        transport.setSelectorRunnersCount(Runtime.getRuntime().availableProcessors() * 2);
        
        // set PooledMemoryManager
        transport.setMemoryManager(new PooledMemoryManager());
        
        // always keep-alive
        networkListener.getKeepAlive().setIdleTimeoutInSeconds(-1);
        networkListener.getKeepAlive().setMaxRequestsCount(-1);
        
        // disable transaction timeout
        networkListener.setTransactionTimeout(-1);
        
        // remove the features we don't need
        networkListener.registerAddOn(new SimplifyAddOn());
        // add HTTP pipeline optimization
        networkListener.registerAddOn(new HttpPipelineOptAddOn());
        
        // disable file-cache
        networkListener.getFileCache().setEnabled(false);
        
        httpServer.addListener(networkListener);
        
        httpServer.getServerConfiguration().addHttpHandler(
                new RootHttpHandler(), "/");
//        httpServer.getServerConfiguration().addHttpHandler(
//                new PlainTextHttpHandler(), "/plaintext");
//        httpServer.getServerConfiguration().addHttpHandler(
//                new JsonHttpHandler(), "/json");
        
        try {
            httpServer.start();
            
            System.err.print("Server started.\n");
            synchronized (Server.class) {
		Server.class.wait();
            }
        } finally {
            httpServer.shutdown();
        }
    }
    
    private static class SimplifyAddOn implements AddOn {

        @Override
        public void setup(final NetworkListener networkListener,
                final FilterChainBuilder builder) {
            final int fcIdx = builder.indexOfType(FileCacheFilter.class);
            if (fcIdx != -1) {
                builder.remove(fcIdx);
            }
            
            final int itIdx = builder.indexOfType(IdleTimeoutFilter.class);
            if (itIdx != -1) {
                builder.remove(itIdx);
            }
        }
    }    
}
