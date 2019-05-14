package hello;

import java.io.Closeable;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.ws.rs.ext.Provider;

import org.glassfish.hk2.api.Factory;
import org.glassfish.jersey.server.CloseableService;

@Provider
public class EMFactory implements Factory<EntityManager> {
	private final CloseableService closeableService;
    EntityManagerFactory emf;

	@Inject
    public EMFactory(CloseableService closeableService) {
        this.closeableService = closeableService;
        emf = Persistence.createEntityManagerFactory("0-jersey");
    }

    @Override
    public void dispose(EntityManager em) {
        em.close();
    }

    @Override
    public EntityManager provide() {
        final EntityManager em = emf.createEntityManager();
        closeableService.add(new Closeable() {
            public final void close() {
                em.close();
            }
        });
        return em;
    }
}