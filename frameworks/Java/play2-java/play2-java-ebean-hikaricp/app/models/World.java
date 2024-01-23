package models;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import io.ebean.DB;
import io.ebean.Finder;
import io.ebean.Model;
import io.ebean.Transaction;

@Entity
public class World extends Model {

    private static final Finder<Long, World> find = new Finder<>(World.class);

    @Id
    public Long id;

    @Column(name = "randomNumber")
    public Long randomNumber;

    public static World find(final Long id) {
        return find.byId(id);
    }

    public static List<World> save(final List<World> worlds) {
        final int batchSize = 25;
        final int batches = ((worlds.size() / batchSize) + 1);
        for ( int i = 0 ; i < batches ; ++i ) {
            final Transaction transaction = DB.getDefault().beginTransaction();
            try {
                transaction.setBatchMode(true);
                transaction.setBatchSize(batchSize);
                for(int j = i * batchSize ; j < Math.min((i + 1) * batchSize, worlds.size()); ++j) {
                    DB.getDefault().update(worlds.get(j), transaction);
                }
                transaction.commit();
            } finally {
                transaction.end();
            }
        }

        return worlds;
    }
}
