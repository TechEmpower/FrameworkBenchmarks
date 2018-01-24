package com.techempower.ee7.tests;

import java.util.ArrayList;
import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@RequestScoped
public class TestActions {

    @Inject
    private EntityManager em;

    @Transactional
    public List<World> updateWorlds(int iterations) {
        List<World> worlds = new ArrayList<>(iterations);

        for (int i = 0; i < iterations; i++) {
            int id = Helpers.randomWorldId();
            worlds.add(em.find(World.class, id));
        }

        for (World w : worlds) {
            w.getRandomNumber(); // Mandatory to read for the test
            w.setRandomNumber(Helpers.randomWorldId());
        }
        return worlds;
    }
}
