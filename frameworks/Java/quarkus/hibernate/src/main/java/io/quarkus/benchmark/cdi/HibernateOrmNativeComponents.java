package io.quarkus.benchmark.cdi;

import javax.enterprise.inject.Produces;
import javax.inject.Singleton;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;

import org.hibernate.SessionFactory;

@Singleton
public class HibernateOrmNativeComponents {

	@PersistenceUnit
	EntityManagerFactory entityManagerFactory;

	@Produces
	SessionFactory extractSessionFactory() {
		return entityManagerFactory.unwrap( SessionFactory.class );
	}

}
