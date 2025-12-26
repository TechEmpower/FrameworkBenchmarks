package hello;

import java.util.concurrent.ScheduledExecutorService;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.SocketChannel;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {

    @Override
    protected void initChannel(SocketChannel ch) {
        ScheduledExecutorService service = ch.eventLoop();
        ch.pipeline().addLast("handler", new HelloServerHandler(service));
    }
}
