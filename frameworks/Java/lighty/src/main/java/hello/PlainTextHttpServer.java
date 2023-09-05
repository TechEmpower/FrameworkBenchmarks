package hello;

import io.github.light0x00.lighty.codec.http.HttpMessageDecoder;
import io.github.light0x00.lighty.codec.http.HttpMessageEncoder;
import io.github.light0x00.lighty.core.eventloop.NioEventLoopGroup;
import io.github.light0x00.lighty.core.facade.ServerBootstrap;

import java.net.InetSocketAddress;

/**
 * @author light0x00
 * @since 2023/9/5
 */
public class PlainTextHttpServer {
    public static void main(String[] args) {
        NioEventLoopGroup group = new NioEventLoopGroup(Runtime.getRuntime().availableProcessors());

        new ServerBootstrap()
                .group(group)
                .childInitializer(channel -> {
                    channel.pipeline()
                            .add(
                                    new HttpMessageDecoder(),
                                    new HttpMessageEncoder(),
                                    new HttpHelloWorldHandler()
                            );
                })
                .bind(new InetSocketAddress(8080));
    }
}
