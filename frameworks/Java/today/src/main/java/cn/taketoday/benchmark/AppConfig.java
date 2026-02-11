package cn.taketoday.benchmark;

import java.time.ZonedDateTime;

import infra.beans.factory.annotation.DisableAllDependencyInjection;
import infra.beans.factory.config.BeanDefinition;
import infra.context.annotation.Configuration;
import infra.context.annotation.Role;
import infra.stereotype.Component;
import infra.web.server.WebServerFactoryCustomizer;
import infra.web.server.netty.NettyWebServerFactory;
import infra.web.server.netty.config.NettyRequestConfigCustomizer;
import io.netty.channel.IoHandlerFactory;
import io.netty.channel.MultiThreadIoEventLoopGroup;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.uring.IoUring;
import io.netty.channel.uring.IoUringIoHandler;
import io.netty.channel.uring.IoUringServerSocketChannel;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpHeadersFactory;
import io.netty.util.concurrent.DefaultThreadFactory;

import static infra.http.HttpHeaders.DATE_FORMATTER;

/**
 * @author <a href="https://github.com/TAKETODAY">Harry Yang</a>
 * @since 1.0 2024/3/19 12:59
 */
@DisableAllDependencyInjection
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
@Configuration(proxyBeanMethods = false)
class AppConfig {

  @Component
  public static WebServerFactoryCustomizer<NettyWebServerFactory> factoryWebServerFactoryCustomizer() {
    return factory -> {
      if (IoUring.isAvailable()) {
        factory.setSocketChannel(IoUringServerSocketChannel.class);

        IoHandlerFactory ioHandlerFactory = IoUringIoHandler.newFactory();
        if (factory.getAcceptorGroup() == null) {
          factory.setAcceptorGroup(new MultiThreadIoEventLoopGroup(factory.getAcceptorThreadCount(),
                  new DefaultThreadFactory("uring-acceptor"), ioHandlerFactory));
        }
        if (factory.getWorkerGroup() == null) {
          factory.setWorkerGroup(new MultiThreadIoEventLoopGroup(
                  factory.getWorkThreadCount(), new DefaultThreadFactory("uring-workers"), ioHandlerFactory));
        }
      }

      System.out.println("IoUring: " + IoUring.isAvailable());
      System.out.println("Epoll: " + Epoll.isAvailable());
    };
  }

  @Component
  @Role(BeanDefinition.ROLE_INFRASTRUCTURE)
  static NettyRequestConfigCustomizer nettyRequestConfigCustomizer() {
    return builder -> builder.headersFactory(new HttpHeadersFactory() {

      @Override
      public HttpHeaders newHeaders() {
        HttpHeaders headers = new ResponseHeaders();
        headers.set("Server", "TODAY");
        headers.set("Date", DATE_FORMATTER.format(ZonedDateTime.now()));
        return headers;
      }

      @Override
      public HttpHeaders newEmptyHeaders() {
        return new ResponseHeaders();
      }
    });
  }

  static class ResponseHeaders extends DefaultHttpHeaders {

    public ResponseHeaders() {
      super(name -> { }, v -> { });
    }

  }

}
