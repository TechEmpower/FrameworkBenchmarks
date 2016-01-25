package com.techempower.beyondj.action;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
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

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadLocalRandom;

@UrlBinding("/perf/database/{_eventName}/{queries}")
public class WorldDatabaseActionBean extends BaseActionBean {

    @Validate(required = false)
    private String queries;

    @HandlesEvent(DB)
    @DefaultHandler
    public Resolution queryOne() throws Exception{

        final Random random = ThreadLocalRandom.current();
        World world = worldRepository.findOne(random.nextInt(DB_ROWS) + 1);

        if(world.getRandomNumber() ==null){
            world.setRandomNumber(ThreadLocalRandom.current().nextInt(DB_ROWS) + 1);
        }

        Gson gson = new GsonBuilder()
                .enableComplexMapKeySerialization()
                .serializeNulls()
                .setPrettyPrinting()
                .setVersion(1.0)
                .excludeFieldsWithoutExposeAnnotation()
                .create();

        String rawJsonText = gson.toJson(world);

        Map<String, String> headers = new HashMap<>();
        headers.put(CONTENT_LENGTH, String.valueOf(rawJsonText.getBytes().length));
        getContext().getResponse().setCharacterEncoding("UTF-8");
        getContext().getRequest().setCharacterEncoding("UTF-8");
        getContext().getResponse().setContentType("application/json");
        setResponseHeaders(headers);
        return new JsonResolution(rawJsonText);
    }

    @HandlesEvent(QUERIES)
    @Transactional
    public Resolution queries() throws Exception {
        int value = boundQueryCount();
        List<Future<World>> wfs = new ArrayList<>(value);
        for (int i = 0; i < value; i++) {
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
        List<World> worlds = waitFor(wfs);

        Gson gson = new GsonBuilder()
                .enableComplexMapKeySerialization()
                .serializeNulls()
                .setPrettyPrinting()
                .setVersion(1.0)
                .excludeFieldsWithoutExposeAnnotation()
                .create();

        String rawJsonText = gson.toJson(worlds);
        Map<String, String> headers = new HashMap<>();
        headers.put(CONTENT_LENGTH, String.valueOf(rawJsonText.getBytes().length));
        getContext().getResponse().setCharacterEncoding("UTF-8");
        getContext().getRequest().setCharacterEncoding("UTF-8");
        getContext().getResponse().setContentType("application/json");
        setResponseHeaders(headers);
        return new JsonResolution(rawJsonText);
    }

    private int extractQueriesValue() {
        int queriesValue = 1;
        try {
            queriesValue = Integer.valueOf(queries);
        } catch (Exception e) {
            //do nothing
        }
        return queriesValue;
    }

    @HandlesEvent(UPDATES)
    @Transactional
    public Resolution updates() throws Exception{
        int value = boundQueryCount();

        List<Future<World>> wfs = new ArrayList<>(value);
        for (int i = 0; i < value; i++) {
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

        Gson gson = new GsonBuilder()
                .enableComplexMapKeySerialization()
                .serializeNulls()
                .setPrettyPrinting()
                .setVersion(1.0)
                .excludeFieldsWithoutExposeAnnotation()
                .create();

        String rawJsonText = gson.toJson(worlds);
        Map<String, String> headers = new HashMap<>();
        headers.put(CONTENT_LENGTH, String.valueOf(rawJsonText.getBytes().length));
        getContext().getResponse().setCharacterEncoding("UTF-8");
        getContext().getRequest().setCharacterEncoding("UTF-8");
        getContext().getResponse().setContentType("application/json");
        setResponseHeaders(headers);
        return new JsonResolution(rawJsonText);
    }

    public String getQueries() {
        return queries;
    }

    public void setQueries(String queries) {
        this.queries = queries;
    }

    private List<World> waitFor(List<Future<World>> wfs) {
        List<World> worlds = new ArrayList<>(wfs.size());
        for (Future<World> wf : wfs) {
            try {

                World world = wf.get();
                if(world.getRandomNumber() ==null){
                    world.setRandomNumber(ThreadLocalRandom.current().nextInt(DB_ROWS) + 1);
                }
                worlds.add(world);
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        }
        return worlds;
    }

    private int boundQueryCount() {
        int queriesValue = extractQueriesValue();
        if (queriesValue < 1) {
            queriesValue = 1;
        } else if (queriesValue > 500) {
            queriesValue = 500;
        }
        return queriesValue;
    }

    private static final int DB_ROWS = 10000;

    @SpringBean
    private WorldRepository worldRepository;

    private static final String DB = "db";
    private static final String QUERIES = "queries";
    private static final String UPDATES = "updates";
    public static final String CONTENT_LENGTH = "Content-Length";
}

