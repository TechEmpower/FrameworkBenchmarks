package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.World;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;

import java.util.List;

public class WorldRepositoryImpl extends Repository implements WorldRepository {
    @Override
    public World findOne(Integer id) {
        return (World) createCriteria(World.class)
                .add(Restrictions.eq(ID, id)).uniqueResult();
    }

    @Override
    public long count() {
        return (Integer) getSession().createCriteria(World.class)
                .setProjection(Projections.rowCount()).uniqueResult();
    }

    public List<World> findAll() {
        return (List<World>) createQuery(
                "from World").list();
    }

    @Override
    public void save(World obj) {
        getSession().persist(obj);
    }
}
