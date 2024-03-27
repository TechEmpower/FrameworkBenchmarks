package cn.taketoday.benchmark.http;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import cn.taketoday.benchmark.model.Fortune;
import cn.taketoday.benchmark.model.World;
import cn.taketoday.http.MediaType;
import cn.taketoday.http.ResponseEntity;
import cn.taketoday.jdbc.persistence.EntityManager;
import cn.taketoday.lang.Nullable;
import cn.taketoday.ui.Model;
import cn.taketoday.web.annotation.GET;
import cn.taketoday.web.annotation.RestController;
import cn.taketoday.web.view.ViewRef;

/**
 * @author <a href="https://github.com/TAKETODAY">Harry Yang</a>
 * @since 1.0 2024/3/19 12:56
 */
@RestController
final class BenchmarkHttpHandler {

  private static final int MIN_WORLD_NUMBER = 1;
  private static final int MAX_WORLD_NUMBER = 10_000;

  private final EntityManager entityManager;

  BenchmarkHttpHandler(EntityManager entityManager) {
    this.entityManager = entityManager;
  }

  @GET("/json")
  public ResponseEntity<Map<String, String>> json() {
    return ResponseEntity.ok()
            .contentType(MediaType.APPLICATION_JSON)
            .body(Map.of("message", "Hello, World!"));
  }

  @GET("/plaintext")
  public String plaintext() {
    return "Hello, World!";
  }

  @GET("/db")
  public World db() {
    return entityManager.findById(World.class, nextInt());
  }

  @GET("/queries")
  public List<World> queries(@Nullable String queries) {
    return randomNumbers()
            .mapToObj(this::findWorldById)
            .limit(parseQueryCount(queries))
            .toList();
  }

  @GET("/updates")
  public List<World> updates(@Nullable String queries) {
    return randomNumbers()
            .mapToObj(this::findWorldById)
            .filter(Objects::nonNull)
            .peek(world -> {
              world.setRandomNumber(nextInt());
              entityManager.updateById(world);
            })
            .limit(parseQueryCount(queries))
            .toList();
  }

  @GET("/fortunes")
  public ViewRef fortunes(Model model) {
    List<Fortune> fortunes = entityManager.find(Fortune.class);
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    model.addAttribute("fortunes", fortunes);
    return ViewRef.forViewName("fortunes");
  }

  @Nullable
  private World findWorldById(int id) {
    return entityManager.findById(World.class, boxed[id]);
  }

  //

  private static IntStream randomNumbers() {
    return ThreadLocalRandom.current()
            .ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER)
            .distinct();
  }

  private static final Integer[] boxed = IntStream.range(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER + 1)
          .boxed()
          .toArray(Integer[]::new);

  private static Integer nextInt() {
    return boxed[ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER)];
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
