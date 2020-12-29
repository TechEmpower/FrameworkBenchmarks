package io.quarkus.benchmark.resource;

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.WorldRepository;
import io.quarkus.vertx.web.Route;
import io.smallrye.mutiny.Uni;
import io.smallrye.mutiny.groups.UniAndGroupIterable;
import io.vertx.ext.web.RoutingContext;


@ApplicationScoped
public class DbResource extends BaseResource {

    @Inject
    WorldRepository worldRepository;

    @Route(path = "db")
    public void db(RoutingContext rc) {
        randomWorld().subscribe().with(world -> sendJson(rc, world),
                                       t -> handleFail(rc, t));
    }

    @Route(path = "queries")
    public void queries(RoutingContext rc) {
        var queries = rc.request().getParam("queries");
        var worlds = new Uni[parseQueryCount(queries)];
        var ret = new World[worlds.length];
        Arrays.setAll(worlds, i -> {
            return randomWorld().map(w -> ret[i] = w);
        });

        Uni.combine().all().unis(worlds)
        .combinedWith(v -> Arrays.asList(ret))
        .subscribe().with(list -> sendJson(rc, list),
                          t -> handleFail(rc, t));
    }

    @Route(path = "updates")
    public void updates(RoutingContext rc) {
        var queries = rc.request().getParam("queries");
        var worlds = new Uni[parseQueryCount(queries)];
        var ret = new World[worlds.length];
        Arrays.setAll(worlds, i -> {
            return randomWorld().map(w -> {
                w.setRandomNumber(randomWorldNumber());
                ret[i] = w;
                return w;
            });
        });

        Uni.combine().all().unis(worlds)
        .combinedWith(v -> null)
        .flatMap(v -> worldRepository.update(ret))
        .map(v -> Arrays.asList(ret))
        .subscribe().with(list -> sendJson(rc, list),
                          t -> handleFail(rc, t));
    }

    private Uni<World> randomWorld() {
        return worldRepository.find(randomWorldNumber());
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}