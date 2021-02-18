package benchmark;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import benchmark.repository.DbRepository;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.micronaut.views.rocker.RockerWritable;
import io.reactivex.Flowable;
import io.reactivex.Single;
import benchmark.views.fortunes;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Controller("/")
public class Database {

    private final DbRepository dbRepository;

    public Database(DbRepository dbRepository) {
        this.dbRepository = dbRepository;
    }

    @Get("/db")
    public Single<World> db() {
        return dbRepository.getWorld(randomWorldNumber());
    }

    @Get("/queries")
    public Single<List<World>> queries(@QueryValue String queries) {
        Flowable<World>[] worlds = new Flowable[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> dbRepository.getWorld(randomWorldNumber()).toFlowable());

        return Flowable.merge(Arrays.asList(worlds)).toList();
    }

    @Get(value = "/fortunes")
    public Single<HttpResponse<RockerWritable>> fortune() {
        return dbRepository.fortunes().toList().map(fortuneList -> {
            fortuneList.add(new Fortune(0, "Additional fortune added at request time."));
            fortuneList.sort(comparing(fortune -> fortune.message));
            return HttpResponse.ok(new RockerWritable(fortunes.template(fortuneList)))
                    .contentType("text/html;charset=utf-8");
        });
    }

    @Get("/updates")
    public Single<List<World>> updates(@QueryValue String queries) {
        Flowable<World>[] worlds = new Flowable[parseQueryCount(queries)];

        Arrays.setAll(worlds, i ->
                dbRepository.findAndUpdateWorld(randomWorldNumber(), randomWorldNumber()).toFlowable()
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
