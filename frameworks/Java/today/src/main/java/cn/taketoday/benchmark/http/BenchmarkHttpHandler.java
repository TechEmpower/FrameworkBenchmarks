package cn.taketoday.benchmark.http;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import cn.taketoday.benchmark.model.Fortune;
import cn.taketoday.benchmark.model.Message;
import cn.taketoday.benchmark.model.World;
import infra.http.MediaType;
import infra.http.ResponseEntity;
import infra.lang.Nullable;
import infra.persistence.EntityManager;
import infra.ui.Model;
import infra.util.concurrent.Future;
import infra.web.annotation.GET;
import infra.web.annotation.RestController;
import infra.web.view.ViewRef;

/**
 * @author <a href="https://github.com/TAKETODAY">Harry Yang</a>
 * @since 1.0 2024/3/19 12:56
 */
@RestController
final class BenchmarkHttpHandler {

  private static final int MIN_WORLD_NUMBER = 1;

  private static final int MAX_WORLD_NUMBER = 10_000;

  private final EntityManager entityManager;

  private final WorldCache worldCache;

  BenchmarkHttpHandler(EntityManager entityManager) {
    this.entityManager = entityManager;
    this.worldCache = new WorldCache(entityManager.find(World.class));
  }

  @GET("/json")
  public ResponseEntity<Message> json() {
    return ResponseEntity.ok()
            .contentType(MediaType.APPLICATION_JSON)
            .body(new Message("Hello, World!"));
  }

  @GET("/plaintext")
  public String plaintext() {
    return "Hello, World!";
  }

  @Nullable
  @GET("/db")
  public World db() {
    return entityManager.findById(World.class, nextInt());
  }

  @GET("/queries")
  public Future<List<World>> queries(@Nullable String queries) {
    return Future.combine(randomNumbers().limit(parseQueryCount(queries)).mapToObj(this::findWorldByIdFuture))
            .asList();
  }

  @GET("/cached-queries")
  public List<World> cachedQueries(@Nullable String count) {
    return worldCache.getCachedWorld(parseQueryCount(count));
  }

  @GET("/updates")
  public Future<List<World>> updates(@Nullable String queries) {
    return Future.combine(randomNumbers()
            .limit(parseQueryCount(queries))
            .mapToObj(this::findWorldByIdFuture)
            .map(worldFuture -> worldFuture.map(world -> {
              world.setRandomNumber(nextInt());
              entityManager.updateById(world);
              return world;
            }))).asList();
  }

  @GET("/fortunes")
  public ViewRef fortunes(Model model) {
    List<Fortune> fortunes = entityManager.find(Fortune.class);
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    model.addAttribute("fortunes", fortunes);
    return ViewRef.forViewName("fortunes");
  }

  private Future<World> findWorldByIdFuture(int id) {
    return Future.run(() -> findWorldById(id));
  }

  @Nullable
  private World findWorldById(int id) {
    return entityManager.findById(World.class, id);
  }

  //

  private static IntStream randomNumbers() {
    return ThreadLocalRandom.current()
            .ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER)
            .distinct();
  }

  private static int nextInt() {
    return ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER);
  }

  private static int parseQueryCount(@Nullable String textValue) {
    if (textValue == null) {
      return 1;
    }
    int parsedValue;
    try {
      parsedValue = Integer.parseInt(textValue);
    }
    catch (NumberFormatException e) {
      return 1;
    }
    return Math.min(500, Math.max(1, parsedValue));
  }

}
