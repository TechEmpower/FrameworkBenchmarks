package io.quarkus.benchmark.repository;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import jakarta.transaction.Transactional;

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

    public World loadSingleWorldById(Integer id) {
        try (StatelessSession ss = sf.openStatelessSession()) {
            return (World) ss.get(World.class, id);
        }
    }

    public World[] loadNWorlds(final int count) {
        final World[] list = new World[count];
        final LocalRandom random = Randomizer.current();
        try (StatelessSession ss = sf.openStatelessSession()) {
            //The rules require individual load: we can't use the Hibernate feature which allows load by multiple IDs as one single operation
            for (int i=0;i<count;i++) {
                list[i] = (World) ss.get(World.class, random.getNextRandom());
            }
            return list;
        }
    }

    public World[] updateNWorlds(final int count) {
        //We're again forced to use the "individual load" pattern by the rules:
        final World[] list = loadNWorlds(count);
        final LocalRandom random = Randomizer.current();
        try (Session s = sf.openSession()) {
            s.setJdbcBatchSize(count);
            s.setHibernateFlushMode(FlushMode.MANUAL);
            for (World w : list) {
                //Read the one field, as required by the following rule:
                // # vi. At least the randomNumber field must be read from the database result set.
                final int previousRead = w.getRandomNumber();
                //Update it, but make sure to exclude the current number as Hibernate optimisations would otherwise
                //skip the write operation:
                w.setRandomNumber(random.getNextRandomExcluding(previousRead));
                s.update(w);
            }
            s.flush();
        }
        return list;
    }

}
