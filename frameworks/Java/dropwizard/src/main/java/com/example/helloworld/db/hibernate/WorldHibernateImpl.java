package com.example.helloworld.db.hibernate;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;
import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

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
        for (int i = 0; i < totalQueries; i++) {
            Transaction transaction = currentSession().beginTransaction();
            try {
                worlds[i] = findAndModify(Helper.randomWorld(), Helper.randomWorld());
                transaction.commit();
            } catch (Exception e) {
                transaction.rollback();
                throw new RuntimeException(e);
            }
        }
        return worlds;
    }

}
