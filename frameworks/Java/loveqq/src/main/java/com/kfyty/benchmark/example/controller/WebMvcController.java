package com.kfyty.benchmark.example.controller;

import com.kfyty.benchmark.example.model.Fortune;
import com.kfyty.benchmark.example.model.Fortunes;
import com.kfyty.benchmark.example.model.World;
import com.kfyty.benchmark.example.repository.DbRepository;
import com.kfyty.benchmark.example.utils.Utils;
import com.kfyty.loveqq.framework.web.core.annotation.GetMapping;
import com.kfyty.loveqq.framework.web.core.annotation.RestController;
import com.kfyty.loveqq.framework.web.core.annotation.bind.RequestParam;
import com.kfyty.loveqq.framework.web.core.http.ServerResponse;
import io.jstach.jstachio.JStachio;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

@RestController
public class WebMvcController {
    private static final byte[] TEXT_BODY = "Hello, World!".getBytes(StandardCharsets.UTF_8);

    private final DbRepository dbRepository;

    public WebMvcController(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    /**
     * GET /plaintext HTTP/1.1
     */
    @GetMapping(value = "/plaintext", produces = "text/plain; charset=UTF-8")
    public byte[] plaintext(ServerResponse response) {
        response.setHeader("Content-Length", "13");
        return TEXT_BODY;
    }

    /**
     * GET /json HTTP/1.1
     */
    @GetMapping(value = "/json")
    public Map<String, String> json(ServerResponse response) {
        response.setHeader("Content-Length", "27");
        return Map.of("message", "Hello, world!");
    }

    /**
     * GET /db HTTP/1.1
     */
    @GetMapping("/db")
    public World db() {
        return dbRepository.getWorld(Utils.randomWorldNumber());
    }

    /**
     * GET /queries?queries=10 HTTP/1.1
     */
    @GetMapping("/queries")
    public World[] queries(@RequestParam(defaultValue = "1") Integer queries) {
        return Utils.randomWorldNumbers().mapToObj(dbRepository::getWorld).limit(queries).toArray(World[]::new);
    }

    /**
     * GET /updates?queries=10 HTTP/1.1
     */
    @GetMapping("/updates")
    public List<World> updates(@RequestParam(defaultValue = "1") Integer queries) {
        List<World> worlds = Utils.randomWorldNumbers()
                .mapToObj(id -> {
                    World world = dbRepository.getWorld(id);
                    int randomNumber;
                    do {
                        randomNumber = Utils.randomWorldNumber();
                    } while (randomNumber == world.randomNumber);
                    world.randomNumber = randomNumber;
                    return world;
                })
                .limit(queries)
                .sorted(Comparator.comparingInt(w -> w.id))
                .toList();
        dbRepository.updateWorlds(worlds);
        return worlds;
    }

    /**
     * GET /fortunes HTTP/1.1
     */
    @GetMapping(value = "/fortunes", produces = "text/html; charset=UTF-8")
    public String fortunes() {
        List<Fortune> fortunes = dbRepository.fortunes();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));

        Collections.sort(fortunes);

        return JStachio.render(new Fortunes(fortunes));
    }
}
