package com.techempower.ee7.util;

import javax.enterprise.inject.Produces;
import javax.enterprise.inject.spi.InjectionPoint;

import org.jboss.logging.Logger;

public class LoggerProducer {

  @Produces
  Logger produceLog(InjectionPoint injectionPoint) {
    return Logger.getLogger(injectionPoint.getBean() != null ? injectionPoint.getBean()
        .getBeanClass() : injectionPoint.getMember().getDeclaringClass());
  }
}
