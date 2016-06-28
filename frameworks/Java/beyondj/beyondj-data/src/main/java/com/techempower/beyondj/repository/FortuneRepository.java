package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.Fortune;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import java.util.List;

public class FortuneRepository {

    @PersistenceContext
    private EntityManager entityManager;

    public Fortune findOne(Integer id) {
        return entityManager.find(Fortune.class, id);
    }

    public long count() {
        CriteriaBuilder qb = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> cq = qb.createQuery(Long.class);
        cq.select(qb.count(cq.from(Fortune.class)));
        return entityManager.createQuery(cq).getSingleResult();
    }

    public List<Fortune> findAll() {
        return entityManager.createQuery("Select t from " + Fortune.class.getSimpleName() + " t").getResultList();
    }

    @Transactional
    public void save(Fortune obj) {
        entityManager.merge(obj);
    }
}
