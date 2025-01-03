package org.jetbrains.ktor.benchmarks;

import io.netty.channel.Channel;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.DatagramChannel;
import io.netty.channel.socket.ServerSocketChannel;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioDatagramChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;
import io.netty.util.concurrent.DefaultThreadFactory;
import io.netty.util.concurrent.Future;
import io.netty.util.concurrent.ThreadPerTaskExecutor;
import reactor.core.publisher.Mono;
import reactor.netty.FutureMono;
import reactor.netty.resources.LoopResources;

import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Copied from GitHub issue comment: https://github.com/r2dbc/r2dbc-pool/issues/190#issuecomment-1566845190
 */
public class NioClientEventLoopResources implements LoopResources {
    public static final String THREAD_PREFIX = "prefix-";
    final int threads;
    final AtomicReference<EventLoopGroup> loops = new AtomicReference<>();
    final AtomicBoolean running;

    NioClientEventLoopResources(int threads) {
        this.running = new AtomicBoolean(true);
        this.threads = threads;
    }


    @Override
    @SuppressWarnings("unchecked")
    public Mono<Void> disposeLater(Duration quietPeriod, Duration timeout) {
        return Mono.defer(() -> {
        long quietPeriodMillis = quietPeriod.toMillis();
        long timeoutMillis = timeout.toMillis();
        EventLoopGroup serverLoopsGroup = loops.get();
        Mono<?> slMono = Mono.empty();
        if (running.compareAndSet(true, false)) {
            if (serverLoopsGroup != null) {
                slMono = FutureMono.from((Future) serverLoopsGroup.shutdownGracefully(
                        quietPeriodMillis, timeoutMillis, TimeUnit.MILLISECONDS));
            }
        }
        return Mono.when(slMono);
    });
    }

    @Override
    public boolean isDisposed() {
        return !running.get();
    }

    @Override
    public EventLoopGroup onClient(boolean useNative) {
        return cacheLoops();
    }

    @Override
    public EventLoopGroup onServer(boolean useNative) {
        throw new UnsupportedOperationException("This event loop is designed only for client DB calls.");
    }

    @Override
    public EventLoopGroup onServerSelect(boolean useNative) {
        throw new UnsupportedOperationException("This event loop is designed only for client DB calls.");
    }

    @Override
    public <CHANNEL extends Channel> CHANNEL onChannel(Class<CHANNEL> channelType, EventLoopGroup group) {
        if (channelType.equals(SocketChannel.class)) {
                return (CHANNEL) new NioSocketChannel();
            }
            if (channelType.equals(ServerSocketChannel.class)) {
                    return (CHANNEL) new NioServerSocketChannel();
                }
                if (channelType.equals(DatagramChannel.class)) {
                        return (CHANNEL) new NioDatagramChannel();
                    }
                    throw new IllegalArgumentException("Unsupported channel type: " + channelType.getSimpleName());
    }

    @Override
    public <CHANNEL extends Channel> Class<? extends CHANNEL> onChannelClass(Class<CHANNEL> channelType,
        EventLoopGroup group) {
        if (channelType.equals(SocketChannel.class)) {
                return (Class<? extends CHANNEL>) NioSocketChannel.class;
            }
            if (channelType.equals(ServerSocketChannel.class)) {
                    return (Class<? extends CHANNEL>) NioServerSocketChannel.class;
                }
                if (channelType.equals(DatagramChannel.class)) {
                        return (Class<? extends CHANNEL>) NioDatagramChannel.class;
                    }
                    throw new IllegalArgumentException("Unsupported channel type: " + channelType.getSimpleName());
    }

    @SuppressWarnings("FutureReturnValueIgnored")
    EventLoopGroup cacheLoops() {
        EventLoopGroup eventLoopGroup = loops.get();
        if (null == eventLoopGroup) {
            EventLoopGroup newEventLoopGroup = createNewEventLoopGroup();
            if (!loops.compareAndSet(null, newEventLoopGroup)) {
                //"FutureReturnValueIgnored" this is deliberate
                newEventLoopGroup.shutdownGracefully(0, 0, TimeUnit.MILLISECONDS);
            }
            eventLoopGroup = cacheLoops();
        }
        return eventLoopGroup;
    }

    private NioEventLoopGroup createNewEventLoopGroup() {
        return new NioEventLoopGroup(threads, new ThreadPerTaskExecutor(new DefaultThreadFactory(THREAD_PREFIX)));
    }
}