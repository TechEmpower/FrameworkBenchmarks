package benchmark;

import io.micronaut.context.annotation.Property;
import io.micronaut.core.util.SupplierUtil;
import io.micronaut.http.netty.channel.loom.EventLoopVirtualThreadScheduler;
import io.micronaut.http.netty.channel.loom.PrivateLoomSupport;
import io.micronaut.scheduling.LoomSupport;
import io.netty.bootstrap.Bootstrap;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFactory;
import io.netty.channel.EventLoop;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.IoHandlerFactory;
import io.netty.channel.ServerChannel;
import io.netty.channel.socket.DatagramChannel;
import io.netty.channel.socket.InternetProtocolFamily;
import io.netty.util.Attribute;
import io.netty.util.AttributeKey;
import io.netty.util.concurrent.FastThreadLocal;
import io.netty.util.internal.ThreadExecutorMap;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.VertxBuilder;
import io.vertx.core.datagram.DatagramSocketOptions;
import io.vertx.core.net.ClientOptionsBase;
import io.vertx.core.net.NetServerOptions;
import io.vertx.core.spi.transport.Transport;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import jakarta.inject.Singleton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.SocketAddress;
import java.util.List;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@Singleton
public class ConnectionHolder {
    private static final Logger log = LoggerFactory.getLogger(ConnectionHolder.class);
    private final FastThreadLocal<Future<PgConnection>> conn = new FastThreadLocal<>();
    private final AttributeKey<Future<PgConnection>> loomConn = AttributeKey.valueOf("vertx-pg-connection");

    @Property(name = "datasources.default.url") String url;
    @Property(name = "datasources.default.username") String user;
    @Property(name = "datasources.default.password") String password;
    @Property(name = "datasources.default.maximum-pool-size") int maxPoolSize;

    private final Supplier<List<Future<PgConnection>>> pool = SupplierUtil.memoized(() -> {
        Vertx vertx = Vertx.builder()
                .withTransport(vertxTransport())
                .build();
        return IntStream.range(0, maxPoolSize)
                .mapToObj(i -> PgConnection.connect(vertx, connectOptions()))
                .toList();
    });

    public Future<PgConnection> get() {
        if (PrivateLoomSupport.isSupported() &&
                LoomSupport.isVirtual(Thread.currentThread()) &&
                PrivateLoomSupport.getScheduler(Thread.currentThread()) instanceof EventLoopVirtualThreadScheduler sch) {

            Attribute<Future<PgConnection>> attr = sch.attributeMap().attr(loomConn);
            Future<PgConnection> c = attr.get();
            if (c == null) {
                c = connect((EventLoop) sch.eventLoop());
                attr.set(c);
            }
            return c;
        } else if (ThreadExecutorMap.currentExecutor() instanceof EventLoop el) {

            Future<PgConnection> c = conn.get();
            if (c == null) {
                c = connect(el);
                conn.set(c);
            }
            return c;
        } else {
            List<Future<PgConnection>> l = pool.get();
            return l.get(ThreadLocalRandom.current().nextInt(l.size()));
        }
    }

    private Future<PgConnection> connect(EventLoop loop) {
        VertxBuilder builder = Vertx.builder();

        io.vertx.core.transport.Transport original = vertxTransport();
        ExistingTransport mapped = new ExistingTransport(original.implementation(), loop);
        io.vertx.core.transport.Transport tr = new io.vertx.core.transport.Transport() {
            @Override
            public String name() {
                return "ExistingTransport";
            }

            @Override
            public boolean available() {
                return true;
            }

            @Override
            public Throwable unavailabilityCause() {
                return null;
            }

            @Override
            public Transport implementation() {
                return mapped;
            }
        };
        Vertx vertx = builder
                .withTransport(tr)
                .build();
        return PgConnection.connect(vertx, connectOptions());
    }

    private static io.vertx.core.transport.Transport vertxTransport() {
        return Stream.of(io.vertx.core.transport.Transport.IO_URING, io.vertx.core.transport.Transport.NIO)
                .filter(t -> t != null && t.available())
                .findFirst().orElseThrow();
    }

    private PgConnectOptions connectOptions() {
        return PgConnectOptions.fromUri(url.substring(5))
                .setUser(user)
                .setPassword(password)
                .setCachePreparedStatements(true)
                .setPipeliningLimit(1024);
    }

    private record ExistingTransport(Transport transport, EventLoop loop) implements Transport {

        @Override
        public boolean supportsDomainSockets() {
            return transport.supportsDomainSockets();
        }

        @Override
        public boolean supportFileRegion() {
            return transport.supportFileRegion();
        }

        @Override
        public boolean isAvailable() {
            return transport.isAvailable();
        }

        @Override
        public Throwable unavailabilityCause() {
            return transport.unavailabilityCause();
        }

        @Override
        public SocketAddress convert(io.vertx.core.net.SocketAddress address) {
            return transport.convert(address);
        }

        @Override
        public io.vertx.core.net.SocketAddress convert(SocketAddress address) {
            return transport.convert(address);
        }

        @Override
        public IoHandlerFactory ioHandlerFactory() {
            return transport.ioHandlerFactory();
        }

        @Override
        public EventLoopGroup eventLoopGroup(int type, int nThreads, ThreadFactory threadFactory, int ignoredIoRatio) {
            return loop;
        }

        @Override
        public DatagramChannel datagramChannel() {
            return transport.datagramChannel();
        }

        @Override
        public DatagramChannel datagramChannel(InternetProtocolFamily family) {
            return transport.datagramChannel(family);
        }

        @Override
        public ChannelFactory<? extends Channel> channelFactory(boolean domainSocket) {
            return transport.channelFactory(domainSocket);
        }

        @Override
        public ChannelFactory<? extends ServerChannel> serverChannelFactory(boolean domainSocket) {
            return transport.serverChannelFactory(domainSocket);
        }

        @Override
        public void configure(DatagramChannel channel, DatagramSocketOptions options) {
            transport.configure(channel, options);
        }

        @Override
        public void configure(ClientOptionsBase options, int connectTimeout, boolean domainSocket, Bootstrap bootstrap) {
            transport.configure(options, connectTimeout, domainSocket, bootstrap);
        }

        @Override
        public void configure(NetServerOptions options, boolean domainSocket, ServerBootstrap bootstrap) {
            transport.configure(options, domainSocket, bootstrap);
        }
    }
}
