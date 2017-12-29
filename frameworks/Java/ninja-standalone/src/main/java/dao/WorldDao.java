package dao;

import model.World;

import javax.persistence.EntityManager;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;

@Singleton
public class WorldDao {

    @Inject
    Provider<EntityManager> entityManagerProvider;

    public World get(int id) {
        EntityManager entityManager = entityManagerProvider.get();
        return entityManager.find(World.class, id);
    }

    public void put(World world) {
        EntityManager entityManager = entityManagerProvider.get();
        entityManager.persist(world);
    }
}
