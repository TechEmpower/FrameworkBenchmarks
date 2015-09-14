package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.Fortune;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;

import java.util.List;

public class FortuneRepositoryImpl extends Repository implements FortuneRepository {

    @Override
    public Fortune findOne(Integer id) {
        return (Fortune) createCriteria(Fortune.class)
                .add(Restrictions.eq(ID, id)).uniqueResult();
    }

    @Override
    public long count() {
        return (Integer) getSession().createCriteria(Fortune.class)
                .setProjection(Projections.rowCount()).uniqueResult();
    }
    @Override
    public List<Fortune> findAll() {
        return (List<Fortune>) createQuery(
                "from Fortune").list();
    }

    @Override
    public void save(Fortune obj) {
         getSession().persist(obj);
    }
}
