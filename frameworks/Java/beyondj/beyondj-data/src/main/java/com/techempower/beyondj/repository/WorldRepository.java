package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.World;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import java.util.List;

public class WorldRepository {

    @PersistenceContext
    private EntityManager entityManager;

    public World findOne(Integer id) {
        return entityManager.find(World.class, id);
    }

    public long count() {
        CriteriaBuilder qb = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> cq = qb.createQuery(Long.class);
        cq.select(qb.count(cq.from(World.class)));
        return entityManager.createQuery(cq).getSingleResult();
    }

    public List<World> findAll() {
        return entityManager.createQuery("Select t from " + World.class.getSimpleName() + " t").getResultList();
    }

    @Transactional
    public void save(World obj) {
        entityManager.merge(obj);
    }
}
