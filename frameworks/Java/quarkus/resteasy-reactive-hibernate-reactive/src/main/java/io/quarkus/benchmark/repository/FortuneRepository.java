package io.quarkus.benchmark.repository;

import java.util.List;

import jakarta.inject.Singleton;

import io.quarkus.benchmark.model.Fortune;
import io.smallrye.mutiny.Uni;

@Singleton
public class FortuneRepository extends BaseRepository {

    public Uni<List<Fortune>> findAll() {
        return inStatelessSession(
                session -> session.createQuery("SELECT F FROM Fortune F", Fortune.class).getResultList()
        );
    }

}
