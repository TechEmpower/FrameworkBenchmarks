package com.example.helloworld.db.hibernate;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;

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
}
