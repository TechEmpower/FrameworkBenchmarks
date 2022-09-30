package io.quarkus.benchmark.repository;

import java.util.List;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;

import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;

import io.quarkus.benchmark.model.Fortune;

@Singleton
public class FortuneRepository {

    @Inject
    SessionFactory sf;

    public List<Fortune> findAllStateless() {
        try (StatelessSession s = sf.openStatelessSession()) {
            CriteriaBuilder criteriaBuilder = sf.getCriteriaBuilder();
            CriteriaQuery<Fortune> fortuneQuery = criteriaBuilder.createQuery(Fortune.class);
            Root<Fortune> from = fortuneQuery.from(Fortune.class);
            fortuneQuery.select(from);
            return s.createQuery(fortuneQuery).getResultList();
        }
    }
}
