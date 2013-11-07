package com.techempower.spring.web;

import com.techempower.spring.domain.World;
import com.techempower.spring.repository.WorldRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

@RestController
final class WorldDatabaseController {

    private static final int DB_ROWS = 10000;

    @Autowired
    private WorldRepository worldRepository;

    @RequestMapping(value = "/db", produces = "application/json")
    World singleQuery() {
        final Random random = ThreadLocalRandom.current();
        return this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
    }

    @RequestMapping(value = "/queries", produces = "application/json")
    List<World> multipleQueries(@RequestParam("queries") Integer rawQueryCount) {
        Integer queryCount = boundQueryCount(rawQueryCount);

        List<World> worlds = new ArrayList<>(queryCount);
        Random random = ThreadLocalRandom.current();

        for (int i = 0; i < queryCount; i++) {
            worlds.add(this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1));
        }

        return worlds;
    }

    @RequestMapping(value = "/updates", produces = "application/json")
    List<World> updateQueries(@RequestParam("queries") Integer rawQueryCount) {
        Integer queryCount = boundQueryCount(rawQueryCount);

        List<World> worlds = multipleQueries(queryCount);
        Random random = ThreadLocalRandom.current();

        for (World world : worlds) {
            world.setRandomNumber(random.nextInt(DB_ROWS) + 1);
        }

        this.worldRepository.save(worlds);
        return worlds;
    }

    private Integer boundQueryCount(Integer raw) {
        if (raw == null || raw < 1) {
            return 1;
        } else if (raw > 500) {
            return 500;
        }

        return raw;
    }

}
