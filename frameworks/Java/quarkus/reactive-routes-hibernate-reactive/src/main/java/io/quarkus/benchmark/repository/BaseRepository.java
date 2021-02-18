package io.quarkus.benchmark.repository;

import java.util.function.Function;

import javax.inject.Inject;

import org.hibernate.reactive.mutiny.Mutiny;

import io.smallrye.mutiny.Uni;

public class BaseRepository {
    @Inject
    protected Mutiny.SessionFactory sf;

    public <T> Uni<T> inSession(Function<Mutiny.Session, Uni<T>> work){
        return sf.withSession(session -> work.apply(session));
    }

}
