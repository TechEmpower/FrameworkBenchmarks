package benchmark.controller;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import benchmark.repository.EntityRepository;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.micronaut.views.rocker.RockerWritable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import views.fortunes;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Controller("/")
public class EntityController {
    private final EntityRepository entityRepository;

    public EntityController(EntityRepository entityRepository) {
        this.entityRepository = entityRepository;
    }

    @Get("/db")
    public Single<World> db() {
        return entityRepository.getWorld(randomWorldNumber());
    }

    @Get("/queries")
    public Single<List<World>> queries(@QueryValue String queries) {
        final Flowable<World>[] worlds = new Flowable[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> entityRepository.getWorld(randomWorldNumber()).toFlowable());

        return Flowable.merge(Arrays.asList(worlds)).toList();
    }

    @Get(value = "/fortunes")
    public Single<HttpResponse<RockerWritable>> fortune() {
        return entityRepository.fortunes().toList().map(fortuneList -> {
            fortuneList.add(new Fortune(0, "Additional fortune added at request time."));
            fortuneList.sort(comparing(Fortune::getMessage));
            return HttpResponse.ok(new RockerWritable(fortunes.template(fortuneList)))
                    .contentType("text/html;charset=utf-8");
        });
    }

    @Get("/updates")
    public Single<List<World>> updates(@QueryValue String queries) {
        final Flowable<World>[] worlds = new Flowable[parseQueryCount(queries)];

        Arrays.setAll(worlds, i ->
                entityRepository.findAndUpdateWorld(randomWorldNumber(), randomWorldNumber()).toFlowable()
        );

        return Flowable.merge(Arrays.asList(worlds)).toList();
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
