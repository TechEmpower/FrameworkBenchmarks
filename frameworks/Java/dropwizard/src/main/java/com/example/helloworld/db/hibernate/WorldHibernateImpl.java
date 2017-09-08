package com.example.helloworld.db.hibernate;

import io.dropwizard.hibernate.AbstractDAO;

import org.hibernate.SessionFactory;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

public class WorldHibernateImpl extends AbstractDAO<World> implements WorldDAO {
    public WorldHibernateImpl(SessionFactory factory) {
        super(factory);
    }

    public World findById(int id) {
        return get(id);
    }

    public World findAndModify(int id, int newRandomNumber) {
        final World world = get(id);
        world.setRandomNumber(newRandomNumber);
        return persist(world);
    }

    @Override
    public World[] updatesQueries(int totalQueries) {
        final World[] worlds = new World[totalQueries];
        //TODO implement write batching
        for (int i = 0; i < totalQueries; i++) {
                worlds[i] = findAndModify(Helper.randomWorld(), Helper.randomWorld());
        }
        return worlds;
    }

}
