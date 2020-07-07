package io.quarkus.benchmark.repository.hibernate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import javax.inject.Inject;
import javax.inject.Singleton;
import javax.transaction.Transactional;

import io.quarkus.benchmark.model.hibernate.World;
import org.hibernate.FlushMode;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;


@Singleton
public class WorldRepository {

    @Inject
    SessionFactory sf;

    /**
     * This method is not required (nor specified) by the benchmark rules,
     * but is quite handy to seed a local database and be able to experiment
     * with the app locally.
     */
    @Transactional
    public void createData() {
        try (StatelessSession statelessSession = sf.openStatelessSession()) {
            final ThreadLocalRandom random = ThreadLocalRandom.current();
            for (int i=1; i<=10000; i++) {
                final World world = new World();
                world.setId(i);
                world.setRandomNumber(1 + random.nextInt(10000));
                statelessSession.insert(world);
            }
        }
    }

    public World findSingleAndStateless(int id) {
        try (StatelessSession ss = sf.openStatelessSession()) {
            return singleStatelessWorldLoad(ss,id);
        }
    }

    public void updateAll(Collection<World> worlds) {
        try (Session s = sf.openSession()) {
            s.setJdbcBatchSize(worlds.size());
            s.setHibernateFlushMode(FlushMode.MANUAL);
            for (World w : worlds) {
                s.update(w);
            }
            s.flush();
        }
    }

    public Collection<World> findReadonly(Set<Integer> ids) {
        try (StatelessSession s = sf.openStatelessSession()) {
            //The rules require individual load: we can't use the Hibernate feature which allows load by multiple IDs as one single operation
            ArrayList l = new ArrayList<>(ids.size());
            for (Integer id : ids) {
                l.add(singleStatelessWorldLoad(s,id));
            }
            return l;
        }
    }

    private static World singleStatelessWorldLoad(final StatelessSession ss, final Integer id) {
        return (World) ss.get(World.class, id);
    }

}
