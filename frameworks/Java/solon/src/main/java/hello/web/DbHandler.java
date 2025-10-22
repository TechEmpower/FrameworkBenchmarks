package hello.web;

import hello.Utils;
import hello.model.Fortune;
import hello.model.World;
import hello.repository.JdbcDbRepository;
import org.noear.solon.annotation.Component;
import org.noear.solon.annotation.Inject;
import org.noear.solon.core.handle.Context;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Component
public class DbHandler {
    @Inject
    JdbcDbRepository dbRepository;

    public void db(Context ctx) throws Throwable {
        ctx.render(dbRepository.getWorld(Utils.randomWorldNumber()));
    }

    public void queries(Context ctx) throws Throwable {
        int queries = ctx.paramAsInt("queries", 0);

        World[] worlds = Utils.randomWorldNumbers()
                .mapToObj(dbRepository::getWorld).limit(queries)
                .toArray(World[]::new);

        ctx.render(worlds);
    }

    public void updates(Context ctx) throws Throwable {
        int queries = ctx.paramAsInt("queries", 0);

        List<World> worlds = Utils.randomWorldNumbers()
                .mapToObj(id -> {
                    World world = dbRepository.getWorld(id);
                    int randomNumber;
                    do {
                        randomNumber = Utils.randomWorldNumber();
                    } while (randomNumber == world.randomNumber);
                    world.randomNumber = randomNumber;
                    return world;
                }).limit(queries)
                .sorted(Comparator.comparingInt(w -> w.id))
                .toList();
        dbRepository.updateWorlds(worlds);

        ctx.render(worlds);
    }

    public void fortunes(Context ctx) throws Throwable {
        List<Fortune> fortunes = dbRepository.fortunes();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);

        ctx.render(new Fortunes(fortunes));
    }
}
