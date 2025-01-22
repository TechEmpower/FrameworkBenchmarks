package com.techempower.ee7.tests;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@RequestScoped
public class TestActions {

    @Inject
    private EntityManager em;

    @Transactional
    public World updateWorld(int id) {
        World w = em.find(World.class, id);
        w.getRandomNumber(); // Mandatory to read for the test
        w.setRandomNumber(Helpers.randomWorldId());
        return w;
    }
}
