package hello;

import javax.inject.Inject;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.ws.rs.ext.Provider;

import org.glassfish.hk2.api.Factory;

@Provider
public class EMFactory implements Factory<EntityManagerFactory> {
	private EntityManagerFactory emf;

	@Inject
	public EMFactory() {
		emf = Persistence.createEntityManagerFactory("0-TFB");
	}

	@Override
	public void dispose(EntityManagerFactory emf) {
		emf.close();
	}

	@Override
	public EntityManagerFactory provide() {
		return emf;
	}
}