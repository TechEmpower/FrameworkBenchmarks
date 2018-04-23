package hello;

import com.sun.jersey.api.core.ResourceConfig;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;
import com.sun.jersey.spi.resource.Singleton;
import hello.domain.Fortune;
import hello.domain.World;
import javax.ws.rs.core.Context;
import javax.ws.rs.ext.Provider;
import org.hibernate.SessionFactory;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.Configuration;

@Singleton
@Provider
public class SessionFactoryProvider
    extends SingletonTypeInjectableProvider<Context, SessionFactory> {

  public SessionFactoryProvider(@Context ResourceConfig rc) {
    super(SessionFactory.class, createSessionFactory(rc));
  }

  private static SessionFactory createSessionFactory(ResourceConfig rc) {
    Configuration configuration = new Configuration().configure();
//    String url = configuration.getProperty("hibernate.hikari.dataSource.url");
//    url = url.replace(
//        "//localhost:3306/",
//        "//" + rc.getProperty("dbhost") + ":" + rc.getProperty("dbport") + "/");
//    configuration.setProperty("hibernate.hikari.dataSource.url", url);
    configuration.addAnnotatedClass(World.class);
    configuration.addAnnotatedClass(Fortune.class);
    StandardServiceRegistryBuilder builder = new StandardServiceRegistryBuilder();
    builder.applySettings(configuration.getProperties());
    return configuration.buildSessionFactory(builder.build());
  }
}
