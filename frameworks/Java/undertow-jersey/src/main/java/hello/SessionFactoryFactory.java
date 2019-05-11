package hello;

import hello.domain.Fortune;
import hello.domain.World;

import javax.ws.rs.ext.Provider;

import org.glassfish.hk2.api.Factory;
import org.hibernate.SessionFactory;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.Configuration;

@Provider
public class SessionFactoryFactory implements Factory<SessionFactory> {
	private final SessionFactory factory;

	public SessionFactoryFactory() {
		factory = createSessionFactory();
	}

	@Override
	public SessionFactory provide() {
		return factory;
	}

	@Override
	public void dispose(SessionFactory factory) {
		factory.close();
	}

	private static SessionFactory createSessionFactory() {
		Configuration configuration = new Configuration().configure();
		configuration.addAnnotatedClass(World.class);
		configuration.addAnnotatedClass(Fortune.class);
		StandardServiceRegistryBuilder builder = new StandardServiceRegistryBuilder();
		builder.applySettings(configuration.getProperties());
		return configuration.buildSessionFactory(builder.build());
	}
}