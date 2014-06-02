package hello;

import io.netty.channel.ChannelHandler;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;

import java.util.concurrent.ScheduledExecutorService;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {
    private final ChannelHandler httpHandler;
    public HelloServerInitializer(ScheduledExecutorService service) {
        this.httpHandler = new HelloServerHandler(service);
    }
    @Override
    public void initChannel(SocketChannel ch) throws Exception {
        ChannelPipeline p = ch.pipeline();
        p.addLast("encoder", new HttpResponseEncoder());
        p.addLast("decoder", new HttpRequestDecoder(4096, 8192, 8192, false));
        p.addLast("handler", httpHandler);
    }
}
