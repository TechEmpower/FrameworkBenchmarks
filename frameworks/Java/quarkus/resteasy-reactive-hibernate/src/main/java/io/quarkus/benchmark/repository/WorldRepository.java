package io.quarkus.benchmark.repository;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.transaction.Transactional;

import org.hibernate.FlushMode;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.utils.LocalRandom;
import io.quarkus.benchmark.utils.Randomizer;


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
            final LocalRandom random = Randomizer.current();
            for (int i=1; i<=10000; i++) {
                final World world = new World();
                world.setId(i);
                world.setRandomNumber(random.getNextRandom());
                statelessSession.insert(world);
            }
        }
    }

    public World findSingleAndStateless(Integer id) {
        try (StatelessSession ss = sf.openStatelessSession()) {
            return singleStatelessWorldLoad(ss,id);
        }
    }

    @Transactional
    public void updateAll(World[] worlds) {
        try (Session s = sf.openSession()) {
            s.setJdbcBatchSize(worlds.length);
            s.setHibernateFlushMode(FlushMode.MANUAL);
            for (World w : worlds) {
                s.update(w);
            }
            s.flush();
        }
    }

    public World[] findReadonly(int count) {
        try (StatelessSession s = sf.openStatelessSession()) {
            //The rules require individual load: we can't use the Hibernate feature which allows load by multiple IDs as one single operation
            World[] list = new World[count];
            final LocalRandom random = Randomizer.current();
            for (int i=0;i<count;i++) {
                Integer idToLoad = random.getNextRandom();
                list[i] = singleStatelessWorldLoad(s,idToLoad);
            }
            return list;
        }
    }

    private static World singleStatelessWorldLoad(final StatelessSession ss, final Integer id) {
        return (World) ss.get(World.class, id);
    }

}
