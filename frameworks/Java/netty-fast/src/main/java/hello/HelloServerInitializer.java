package hello;

import java.util.concurrent.ScheduledExecutorService;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpDecoderConfig;
import io.netty.handler.codec.http.HttpRequestDecoder;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {

    @Override
    protected void initChannel(SocketChannel ch) {
        ScheduledExecutorService service = ch.eventLoop();
        var config = new HttpDecoderConfig().setMaxInitialLineLength(4096).setMaxHeaderSize(8192).setMaxChunkSize(8192);

        ch.pipeline().addLast("httpDecoder", new HttpRequestDecoder(config));
        ch.pipeline().addLast("handler", new HelloServerHandler(service));
    }
}
