package hello;

import hello.domain.Fortune;
import hello.domain.World;

import javax.ws.rs.core.Context;
import javax.ws.rs.ext.Provider;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.service.ServiceRegistryBuilder;

import com.sun.jersey.api.core.ResourceConfig;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;
import com.sun.jersey.spi.resource.Singleton;

@Provider
@Singleton
public class SessionFactoryProvider extends SingletonTypeInjectableProvider<Context, SessionFactory> {

  public SessionFactoryProvider(@Context final ResourceConfig rc) {
    super(SessionFactory.class, createSessionFactory(rc));
  }
  
  private static SessionFactory createSessionFactory(final ResourceConfig rc) {
    Configuration configuration = new Configuration().configure();
    String url = configuration.getProperty("hibernate.connection.url");
    url = url.replace("//localhost:3306/", "//" + rc.getProperty("dbhost") + ":" + rc.getProperty("dbport") + "/");
    configuration.setProperty("hibernate.connection.url", url);
    configuration.addAnnotatedClass(World.class);
    configuration.addAnnotatedClass(Fortune.class);
    ServiceRegistryBuilder serviceRegistryBuilder = new ServiceRegistryBuilder().applySettings(configuration.getProperties());
    return configuration.buildSessionFactory(serviceRegistryBuilder.buildServiceRegistry());
  }
}
