package io.quarkus.benchmark.cdi;

import jakarta.enterprise.inject.Produces;
import jakarta.enterprise.inject.Typed;
import jakarta.inject.Singleton;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.PersistenceUnit;

import org.hibernate.SessionFactory;

@Singleton
public class HibernateOrmNativeComponents {

	@PersistenceUnit
	EntityManagerFactory entityManagerFactory;

	@Singleton
	@Typed(SessionFactory.class)
	@Produces
	SessionFactory extractSessionFactory() {
		return entityManagerFactory.unwrap( SessionFactory.class );
	}

}
