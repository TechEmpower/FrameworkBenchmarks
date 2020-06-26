package benchmark;

import benchmark.entity.Fortune;
import benchmark.entity.World;
import benchmark.repository.DbRepository;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.micronaut.views.ModelAndView;
import io.micronaut.views.View;
import io.reactivex.Flowable;
import io.reactivex.Single;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
        Arrays.setAll(worlds, i -> db().toFlowable());

        return Flowable.merge(Arrays.asList(worlds)).toList();
    }

    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    public Single<ModelAndView<Map>> fortune() {
        return dbRepository.fortunes().toList().flatMap(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(comparing(fortune -> fortune.message));
            return Single.just(new ModelAndView<>("fortunes", Collections.singletonMap("fortunes", fortunes)));
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
