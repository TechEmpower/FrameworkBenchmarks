package hello.dao;

import hello.model.World;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.persist.Transactional;

public class WorldDao {

    @Inject
    Provider<EntityManager> entitiyManagerProvider;

    @Transactional
    public World get(int id) {

	EntityManager entityManager = entitiyManagerProvider.get();

	Query q = entityManager
		.createQuery("SELECT x FROM World x WHERE x.id = :idParam");
	World world = (World) q.setParameter("idParam", id).getSingleResult();

	return world;
    }
}
