package com.techempower.minijax;

import static java.util.stream.Collectors.*;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Stream;

import javax.enterprise.context.RequestScoped;
import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import javax.persistence.PersistenceContext;

@RequestScoped
public class Dao {

    @PersistenceContext
    private EntityManager em;

    public World getWorldById(final int id) {
        return em.find(World.class, id);
    }

    public World getRandomWorld() {
        return getWorldById(randomWorld());
    }

    public List<World> getWorlds(final int count) {
        return Stream.generate(() -> getWorldById(randomWorld()))
                .limit(clamp(count, 1, 500))
                .collect(toList());
    }

    public void batchUpdate(final List<World> worlds) {
        final EntityTransaction tx = em.getTransaction();
        tx.begin();
        for (final World world : worlds) {
            em.merge(world);
        }
        tx.commit();
    }

    public List<Fortune> getAllFortunes() {
        return em.createNamedQuery("Fortune.getAll", Fortune.class).getResultList();
    }

    public static int randomWorld() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private static int clamp(final int x, final int min, final int max) {
        return Math.min(max, Math.max(min, x));
    }
}
