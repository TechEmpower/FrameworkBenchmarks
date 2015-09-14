package com.techempower.beyondj.action;

import com.techempower.beyondj.Common;
import com.techempower.beyondj.domain.World;
import com.techempower.beyondj.repository.WorldRepository;
import net.sourceforge.stripes.action.DefaultHandler;
import net.sourceforge.stripes.action.HandlesEvent;
import net.sourceforge.stripes.action.Resolution;
import net.sourceforge.stripes.action.UrlBinding;
import net.sourceforge.stripes.integration.spring.SpringBean;
import net.sourceforge.stripes.validation.Validate;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.stripesrest.JsonResolution;

import javax.persistence.EntityManagerFactory;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadLocalRandom;

@UrlBinding("/perf/database/{_eventName}/{queries}")
public class WorldDatabaseActionBean extends BaseActionBean {

    @Validate(required = false)
    private int queries = 1;

    @HandlesEvent(DB)
    @DefaultHandler
    public Resolution queryOne() {
        //  validateRepository();
        final Random random = ThreadLocalRandom.current();
        World world = (World) worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
        setResponseDate();
        return new JsonResolution(world);
    }

    @HandlesEvent(QUERIES)
    @Transactional(readOnly = true)
    public Resolution queries() {
        //    validateRepository();
        boundQueryCount();
        List<Future<World>> wfs = new ArrayList<>(queries);
        for (int i = 0; i < queries; i++) {
            wfs.add(
                    Common.EXECUTOR.submit(
                            new Callable<World>() {
                                @Override
                                public World call() throws Exception {
                                    return (World) worldRepository.findOne(
                                            ThreadLocalRandom.current().nextInt(DB_ROWS) + 1);
                                }
                            }));
        }
        setResponseDate();
        return new JsonResolution(wfs);
    }

    @HandlesEvent(UPDATES)
    @Transactional
    public Resolution updates() {
        //   validateRepository();
        boundQueryCount();
        List<Future<World>> wfs = new ArrayList<>(queries);
        for (int i = 0; i < queries; i++) {
            wfs.add(Common.EXECUTOR.submit(
                    new Callable<World>() {
                        @Override
                        @Transactional(propagation = Propagation.REQUIRES_NEW)
                        public World call() throws Exception {
                            Random random = ThreadLocalRandom.current();
                            World world = (World) worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
                            world.setRandomNumber(random.nextInt(DB_ROWS) + 1);
                            worldRepository.save(world);
                            return world;
                        }
                    }));
        }
        List<World> worlds = waitFor(wfs);
        setResponseDate();
        return new JsonResolution(worlds);
    }

    public int getQueries() {
        return queries;
    }

    public void setQueries(int queries) {
        this.queries = queries;
    }

    private List<World> waitFor(List<Future<World>> wfs) {
        List<World> worlds = new ArrayList<>(wfs.size());
        for (Future<World> wf : wfs) {
            try {
                worlds.add(wf.get());
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        }
        return worlds;
    }

    private void boundQueryCount() {
        if (queries < 1) {
            queries = 1;
        } else if (queries > 500) {
            queries = 500;
        }
    }


    private static final int DB_ROWS = 10000;

    @SpringBean
    private EntityManagerFactory entityManagerFactory;

    @SpringBean
    private WorldRepository worldRepository;

    private static final String QUERIES = "queries";
    private static final String DB = "db";
    private static final String UPDATES = "updates";
}
