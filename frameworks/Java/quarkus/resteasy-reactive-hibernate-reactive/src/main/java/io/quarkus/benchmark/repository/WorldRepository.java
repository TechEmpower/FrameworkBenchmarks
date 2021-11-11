package io.quarkus.benchmark.repository;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;

import javax.inject.Singleton;

import org.hibernate.reactive.mutiny.Mutiny;
import org.hibernate.reactive.mutiny.Mutiny.Session;

import io.quarkus.benchmark.model.World;
import io.smallrye.mutiny.Uni;


@Singleton
public class WorldRepository extends BaseRepository {

    /**
     * This method is not required (nor specified) by the benchmark rules,
     * but is quite handy to seed a local database and be able to experiment
     * with the app locally.
     */
    public Uni<Void> createData() {
        return inSession(s -> {
            final ThreadLocalRandom random = ThreadLocalRandom.current();
            int MAX = 10000;
            Uni<Void>[] unis = new Uni[MAX];
            for (int i=0; i<MAX; i++) {
                final World world = new World();
                world.setId(i + 1);
                world.setRandomNumber(1 + random.nextInt(10000));
                unis[i] = s.persist(world).map(v -> null);
            }
            return Uni.combine().all().unis(unis).combinedWith(l -> null)
                    .flatMap(v -> s.flush())
                    .map(v -> null);
        });
    }

    public Uni<World> find(int id) {
        return inSession(session -> singleFind(session, id));
    }

    public Uni<Collection<World>> update(Mutiny.Session s, Collection<World> worlds) {
        return s.flush()
                .map(v -> worlds);
            }

    public Uni<Collection<World>> find(Session s, Set<Integer> ids) {
        //The rules require individual load: we can't use the Hibernate feature which allows load by multiple IDs as one single operation
        ArrayList<Uni<World>> l = new ArrayList<>(ids.size());
        for (Integer id : ids) {
            l.add(singleFind(s, id));
        }
        return Uni.combine().all().unis(l).combinedWith(list -> (List<World>)list);
    }

    private static Uni<World> singleFind(final Mutiny.Session ss, final Integer id) {
        return ss.find(World.class, id);
    }

}
