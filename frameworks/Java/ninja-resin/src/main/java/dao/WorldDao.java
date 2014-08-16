package dao;

import model.World;

import javax.persistence.EntityManager;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;

@Singleton
public class WorldDao {

    @Inject
    Provider<EntityManager> entitiyManagerProvider;

    public World get(int id) {
        EntityManager entityManager = entitiyManagerProvider.get();
        return entityManager.find(World.class, id);
    }

    public void put(World world) {
        EntityManager entityManager = entitiyManagerProvider.get();
        entityManager.persist(world);
    }
}
