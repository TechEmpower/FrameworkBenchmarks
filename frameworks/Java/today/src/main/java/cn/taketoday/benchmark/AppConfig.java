package cn.taketoday.benchmark;

import java.time.ZonedDateTime;

import javax.sql.DataSource;

import cn.taketoday.beans.factory.annotation.DisableAllDependencyInjection;
import cn.taketoday.beans.factory.config.BeanDefinition;
import cn.taketoday.context.annotation.Configuration;
import cn.taketoday.context.annotation.Role;
import cn.taketoday.framework.web.netty.NettyRequestConfig;
import cn.taketoday.framework.web.netty.SendErrorHandler;
import cn.taketoday.jdbc.RepositoryManager;
import cn.taketoday.jdbc.persistence.EntityManager;
import cn.taketoday.stereotype.Component;
import io.netty.handler.codec.http.DefaultHttpHeadersFactory;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpHeadersFactory;
import io.netty.handler.codec.http.multipart.DefaultHttpDataFactory;

import static cn.taketoday.http.HttpHeaders.DATE_FORMATTER;

/**
 * @author <a href="https://github.com/TAKETODAY">Harry Yang</a>
 * @since 1.0 2024/3/19 12:59
 */
@DisableAllDependencyInjection
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
@Configuration(proxyBeanMethods = false)
class AppConfig {

  private static final DefaultHttpHeadersFactory headersFactory = DefaultHttpHeadersFactory.headersFactory();

  @Component
  static RepositoryManager repositoryManager(DataSource dataSource) {
    return new RepositoryManager(dataSource);
  }

  @Component
  static EntityManager entityManager(RepositoryManager repositoryManager) {
    return repositoryManager.getEntityManager();
  }

  @Component
  @Role(BeanDefinition.ROLE_INFRASTRUCTURE)
  static NettyRequestConfig nettyRequestConfig(SendErrorHandler sendErrorHandler) {
    var factory = new DefaultHttpDataFactory(false);
    return NettyRequestConfig.forBuilder()
            .httpDataFactory(factory)
            .sendErrorHandler(sendErrorHandler)
            .headersFactory(new HttpHeadersFactory() {

              @Override
              public HttpHeaders newHeaders() {
                HttpHeaders headers = headersFactory.newHeaders();
                headers.set("Server", "TODAY");
                headers.set("Date", DATE_FORMATTER.format(ZonedDateTime.now()));
                return headers;
              }

              @Override
              public HttpHeaders newEmptyHeaders() {
                return headersFactory.newEmptyHeaders();
              }
            })
            .secure(false)
            .build();
  }

}
