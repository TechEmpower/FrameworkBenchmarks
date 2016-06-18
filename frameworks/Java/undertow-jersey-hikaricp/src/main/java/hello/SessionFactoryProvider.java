package hello;

import hello.domain.*;
import org.glassfish.hk2.api.*;
import org.hibernate.*;
import org.hibernate.cfg.Configuration;
import org.hibernate.service.*;

import javax.ws.rs.core.*;
import javax.ws.rs.core.Context;

public class SessionFactoryProvider
    implements Factory<SessionFactory>
{

  @Context
  Application application;

  public SessionFactoryProvider()
  {
  }

  @Override
  public SessionFactory provide()
  {
    Configuration configuration = new Configuration().configure();
    String url = configuration.getProperty("hibernate.hikari.dataSource.url");
    url = url.replace("//localhost:3306/",
        "//" + this.application.getProperties().get("dbhost") + ":"
            + this.application.getProperties().get("dbport") + "/");
    configuration.setProperty("hibernate.hikari.dataSource.url", url);
    configuration.addAnnotatedClass(World.class);
    configuration.addAnnotatedClass(Fortune.class);
    ServiceRegistryBuilder serviceRegistryBuilder = new ServiceRegistryBuilder().applySettings(
        configuration.getProperties());
    return configuration.buildSessionFactory(
        serviceRegistryBuilder.buildServiceRegistry());
  }

  @Override
  public void dispose(SessionFactory instance)
  {

  }
}
