package io.quarkus.benchmark.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.WorldRepository;
import io.quarkus.benchmark.repository.WorldRepository.JsonWorld;
import io.quarkus.vertx.web.Route;
import io.smallrye.mutiny.Uni;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.web.RoutingContext;
import io.vertx.mutiny.sqlclient.Tuple;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;


@Singleton
public class DbResource extends BaseResource {

    @Inject
    WorldRepository worldRepository;

    @Route(path = "db")
    public void db(final RoutingContext rc) {
        worldRepository.findAsJsonWorld(getRandomTuple())
                .subscribe().with(world -> sendJson(rc, world),
                        t -> handleFail(rc, t));
    }

    @Route(path = "queries")
    public void queries(final RoutingContext rc) {
        final var queries = rc.request().getParam("queries");
        final var worlds = new Uni[parseQueryCount(queries)];
        final var ret = new JsonWorld[worlds.length];
        // replace below with a for loop
        Arrays.setAll(worlds, i -> {
            return worldRepository.findAsJsonWorld(getRandomTuple()).map(w -> ret[i] = w);
        });

        Uni.combine().all().unis(worlds)
                .with(v -> Arrays.asList(ret))
                .subscribe().with(list -> {
                            sendJson(rc, new JsonArray(list));
                        },
                        t -> handleFail(rc, t));
    }

    @Route(path = "updates")
    public void updates(final RoutingContext rc) {
        final var queries = rc.request().getParam("queries");
        final var worlds = new Uni[parseQueryCount(queries)];
        final var wordsToUpdate = new World[worlds.length];
        Arrays.setAll(worlds, i -> {
            return randomWorld().map(w -> {
                w.setRandomNumber(boxedRandomWorldNumber());
                wordsToUpdate[i] = w;
                return w;
            });
        });

        Uni.combine().all().unis(worlds)
                .with(v -> null)
                .flatMap(v -> worldRepository.update(wordsToUpdate))
                .map(updated -> wordsToUpdate)
                .subscribe().with(updatedWordsOrderedById -> {
                            final var jsonWorlds = new JsonArray(new ArrayList<>(updatedWordsOrderedById.length));
                            for (final World world : updatedWordsOrderedById) {
                                jsonWorlds.add(new JsonWorld(world.getId(), world.getRandomNumber()));
                            }
                            sendJson(rc, jsonWorlds);
                        },
                        t -> handleFail(rc, t));
    }

    private Uni<World> randomWorld() {
        return worldRepository.find(getRandomTuple());
    }

    private static final Integer[] BOXED_RND = IntStream.range(1, 10001).boxed().toArray(Integer[]::new);
    private static final Tuple[] tupleCache = new Tuple[10000];

    static {
        for (int i = 0; i < 10000; i++) {
            tupleCache[i] = Tuple.of(i + 1);
        }
    }


    private static Integer boxedRandomWorldNumber() {
        final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
        final var boxedRnd = BOXED_RND[rndValue - 1];
        assert boxedRnd.intValue() == rndValue;
        return boxedRnd;
    }

    private static int primitiveRandomWorldNumber() {
        final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
        return rndValue;
    }

    private static Tuple getRandomTuple() {
        final int rndValue = primitiveRandomWorldNumber();
        final Tuple tuple = tupleCache[rndValue - 1];
        return tuple;
    }

    private static int parseQueryCount(final String textValue) {
        if (textValue == null) {
            return 1;
        }
        final int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (final NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}