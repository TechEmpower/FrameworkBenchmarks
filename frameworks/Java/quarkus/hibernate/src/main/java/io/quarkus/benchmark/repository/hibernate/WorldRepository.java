package io.quarkus.benchmark.repository.hibernate;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import io.quarkus.benchmark.model.hibernate.World;

@ApplicationScoped
public class WorldRepository {

    @Inject
    EntityManager em;

    @Transactional
    public World find(int id) {
        return em.find(World.class, id);
    }

    @Transactional
    public void update(World world) {
        em.merge(world);
    }

    @Transactional
    public void update(World[] worlds) {
        for (World world : worlds) {
            update(world);
        }
    }
}
