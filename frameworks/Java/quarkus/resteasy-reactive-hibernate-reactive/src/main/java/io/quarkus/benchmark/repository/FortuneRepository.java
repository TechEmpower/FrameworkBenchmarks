package io.quarkus.benchmark.repository;

import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import io.quarkus.benchmark.model.Fortune;
import io.smallrye.mutiny.Uni;

@ApplicationScoped
public class FortuneRepository extends BaseRepository {

    public Uni<List<Fortune>> findAll() {
        return inStatelessSession(
                session -> session.createQuery("SELECT F FROM Fortune F", Fortune.class).getResultList()
        );
    }

}
