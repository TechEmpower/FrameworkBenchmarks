package models;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import org.hibernate.Session;

import play.db.jpa.JPAApi;

@Entity
public class World {

    @Id
    public Long id;

    @Column(name = "randomNumber")
    public Long randomNumber;

    public static World findById(final Long id, final JPAApi jpa) {
        return jpa.withTransaction("default", true, em -> { return em.find(World.class, id); });
    }

    public static List<World> save(final List<World> worlds, final JPAApi jpa) {
        final int batchSize = 25;
        final int batches = ((worlds.size() / batchSize) + 1);
        for ( int i = 0 ; i < batches ; ++i ) {
            final int index = i;
            jpa.withTransaction("default", false, em -> {
                em.unwrap(Session.class).setJdbcBatchSize(25);
                for(int j = index * batchSize ; j < Math.min((index + 1) * batchSize, worlds.size()); ++j) {
                    em.merge(worlds.get(j));
                }
                return null;
            });
        }

        return worlds;
    }
}