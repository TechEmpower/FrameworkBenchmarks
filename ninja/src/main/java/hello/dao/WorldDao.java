package hello.dao;

import hello.model.World;

import javax.persistence.EntityManager;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.persist.Transactional;

public class WorldDao {

    @Inject
    Provider<EntityManager> entitiyManagerProvider;

    @Transactional
    public World get(int id) {
	EntityManager entityManager = entitiyManagerProvider.get();
	return entityManager.find(World.class, id);
    }
}
