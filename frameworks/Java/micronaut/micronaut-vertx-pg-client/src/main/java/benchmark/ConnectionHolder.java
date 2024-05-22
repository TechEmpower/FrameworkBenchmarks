package benchmark;

import io.micronaut.context.annotation.Property;
import io.netty.bootstrap.Bootstrap;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFactory;
import io.netty.channel.EventLoop;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.socket.DatagramChannel;
import io.netty.channel.socket.InternetProtocolFamily;
import io.netty.util.concurrent.FastThreadLocal;
import io.netty.util.internal.ThreadExecutorMap;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.datagram.DatagramSocketOptions;
import io.vertx.core.impl.VertxBuilder;
import io.vertx.core.net.ClientOptionsBase;
import io.vertx.core.net.NetServerOptions;
import io.vertx.core.spi.transport.Transport;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.PoolOptions;
import jakarta.inject.Singleton;

import java.net.SocketAddress;
import java.util.concurrent.ThreadFactory;

@Singleton
public class ConnectionHolder {
    private final FastThreadLocal<Future<PgConnection>> conn = new FastThreadLocal<>();

    @Property(name = "datasources.default.url") String url;
    @Property(name = "datasources.default.username") String user;
    @Property(name = "datasources.default.password") String password;
    @Property(name = "datasources.default.maximum-pool-size") int maxPoolSize;

    public Future<PgConnection> get() {
        Future<PgConnection> c = conn.get();
        if (c == null) {

            PgConnectOptions connectOptions = PgConnectOptions.fromUri(url.substring(5))
                    .setUser(user)
                    .setPassword(password)
                    .setCachePreparedStatements(true)
                    .setTcpNoDelay(true)
                    .setTcpQuickAck(true)
                    .setPipeliningLimit(1024);
            PoolOptions poolOptions = new PoolOptions();
            poolOptions.setMaxSize(maxPoolSize);

            VertxBuilder builder = new VertxBuilder()
                    .init();

            EventLoop loop = (EventLoop) ThreadExecutorMap.currentExecutor();

            Vertx vertx = builder
                    .findTransport(new ExistingTransport(builder.findTransport(), loop))
                    .vertx();

            c = PgConnection.connect(vertx, connectOptions);
            conn.set(c);
        }
        return c;
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
            public void configure(ClientOptionsBase options, boolean domainSocket, Bootstrap bootstrap) {
                transport.configure(options, domainSocket, bootstrap);
            }

            @Override
            public void configure(NetServerOptions options, boolean domainSocket, ServerBootstrap bootstrap) {
                transport.configure(options, domainSocket, bootstrap);
            }
        }
}
