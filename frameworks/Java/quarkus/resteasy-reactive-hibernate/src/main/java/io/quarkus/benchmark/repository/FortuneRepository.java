package io.quarkus.benchmark.repository;

import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;
import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;

import io.quarkus.benchmark.model.Fortune;

@Singleton
public class FortuneRepository {

    @Inject
    SessionFactory sf;

    public List<Fortune> findAllStateless() {
        try (StatelessSession s = sf.openStatelessSession()) {
            return s.createQuery("SELECT F FROM Fortune F", Fortune.class).getResultList();
        }
    }
}
