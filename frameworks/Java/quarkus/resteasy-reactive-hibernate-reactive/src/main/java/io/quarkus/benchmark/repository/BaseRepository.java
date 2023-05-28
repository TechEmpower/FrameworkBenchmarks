package io.quarkus.benchmark.repository;

import java.util.function.Function;

import jakarta.inject.Inject;

import org.hibernate.reactive.mutiny.Mutiny;

import io.smallrye.mutiny.Uni;

public class BaseRepository {

    @Inject
    protected Mutiny.SessionFactory sf;

    public <T> Uni<T> inSession(Function<Mutiny.Session, Uni<T>> work) {
        return sf.openSession().chain( session -> work.apply( session ).eventually( session::close ) );
    }

    public <T> Uni<T> inStatelessSession(Function<Mutiny.StatelessSession, Uni<T>> work) {
        return sf.openStatelessSession().chain( session -> work.apply( session ).eventually( session::close ) );
    }

}
