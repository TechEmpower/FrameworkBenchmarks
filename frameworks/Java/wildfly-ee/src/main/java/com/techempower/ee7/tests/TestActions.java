package com.techempower.ee7.tests;

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
    public World updateWorld(int id) {
        World w = em.find(World.class, id);
        w.getRandomNumber(); // Mandatory to read for the test
        w.setRandomNumber(Helpers.randomWorldId());
        return w;
    }
}
