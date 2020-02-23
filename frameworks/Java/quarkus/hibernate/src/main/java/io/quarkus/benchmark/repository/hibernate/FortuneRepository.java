package io.quarkus.benchmark.repository.hibernate;

import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.transaction.Transactional;

import io.quarkus.benchmark.model.hibernate.Fortune;

@ApplicationScoped
public class FortuneRepository {

    @Inject
    EntityManager em;

    @Transactional
    public List<Fortune> findAll() {
        Query query = em.createQuery("SELECT f FROM Fortune f");
        return query.getResultList();
    }
}
