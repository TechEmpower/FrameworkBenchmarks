package cn.taketoday.benchmark;

import java.time.ZonedDateTime;

import javax.sql.DataSource;

import infra.beans.factory.annotation.DisableAllDependencyInjection;
import infra.beans.factory.config.BeanDefinition;
import infra.context.annotation.Configuration;
import infra.context.annotation.Role;
import infra.jdbc.RepositoryManager;
import infra.persistence.EntityManager;
import infra.stereotype.Component;
import infra.web.server.WebServerFactoryCustomizer;
import infra.web.server.error.SendErrorHandler;
import infra.web.server.support.NettyRequestConfig;
import infra.web.server.support.NettyWebServerFactory;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpHeadersFactory;
import io.netty.handler.codec.http.multipart.DefaultHttpDataFactory;
import io.netty.incubator.channel.uring.IOUring;
import io.netty.incubator.channel.uring.IOUringEventLoopGroup;
import io.netty.incubator.channel.uring.IOUringServerSocketChannel;

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
  public static RepositoryManager repositoryManager(DataSource dataSource) {
    return new RepositoryManager(dataSource);
  }

  @Component
  public static EntityManager entityManager(RepositoryManager repositoryManager) {
    return repositoryManager.getEntityManager();
  }

  @Component
  public static WebServerFactoryCustomizer<NettyWebServerFactory> factoryWebServerFactoryCustomizer() {
    return factory -> {
      if (IOUring.isAvailable()) {
        IOUringEventLoopGroup loopGroup = new IOUringEventLoopGroup();
        factory.setAcceptorGroup(loopGroup);
        factory.setWorkerGroup(loopGroup);
        factory.setSocketChannel(IOUringServerSocketChannel.class);
      }
    };
  }

  @Component
  @Role(BeanDefinition.ROLE_INFRASTRUCTURE)
  public static NettyRequestConfig nettyRequestConfig(SendErrorHandler sendErrorHandler) {
    var factory = new DefaultHttpDataFactory(false);

    return NettyRequestConfig.forBuilder(false)
            .httpDataFactory(factory)
            .sendErrorHandler(sendErrorHandler)
            .headersFactory(new HttpHeadersFactory() {

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
            })
            .build();
  }

  static class ResponseHeaders extends DefaultHttpHeaders {

    public ResponseHeaders() {
      super(name -> { }, v -> { });
    }

  }

}
