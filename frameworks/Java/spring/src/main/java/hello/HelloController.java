package hello;

import static java.util.Comparator.comparing;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@EnableAutoConfiguration
public final class HelloController {

  public static void main(String[] args) {
    SpringApplication.run(HelloController.class, args);
  }

  @Autowired
  JdbcTemplate jdbcTemplate;

  @RequestMapping("/plaintext")
  @ResponseBody
  String plaintext() {
    return "Hello, World!";
  }

  @RequestMapping("/json")
  @ResponseBody
  Map<String, String> json() {
    return Map.of("message", "Hello, World!");
  }

  @RequestMapping("/db")
  @ResponseBody
  World db() {
    return randomWorld();
  }

  @RequestMapping("/queries")
  @ResponseBody
  World[] queries(@RequestParam String queries) {
    var worlds = new World[parseQueryCount(queries)];
    Arrays.setAll(worlds, i -> randomWorld());
    return worlds;
  }

  @RequestMapping("/updates")
  @ResponseBody
  World[] updates(@RequestParam String queries) {
    var worlds = new World[parseQueryCount(queries)];
    Arrays.setAll(worlds, i -> randomWorld());
    for (var world : worlds) {
      world.randomNumber = randomWorldNumber();
      jdbcTemplate.update(
          "UPDATE world SET randomnumber = ? WHERE id = ?",
          world.randomNumber,
          world.id);
    }
    return worlds;
  }

  @RequestMapping("/fortunes")
  @ModelAttribute("fortunes")
  List<Fortune> fortunes() {
    var fortunes =
        jdbcTemplate.query(
            "SELECT * FROM fortune",
            (rs, rn) -> new Fortune(rs.getInt("id"), rs.getString("message")));

    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(comparing(fortune -> fortune.message));
    return fortunes;
  }

  private World randomWorld() {
    return jdbcTemplate.queryForObject(
        "SELECT * FROM world WHERE id = ?",
        (rs, rn) -> new World(rs.getInt("id"), rs.getInt("randomnumber")),
        randomWorldNumber());
  }

  private static int randomWorldNumber() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  private static int parseQueryCount(String textValue) {
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

  public static final class Fortune {
    public int id;
    public String message;

    public Fortune(int id, String message) {
      this.id = id;
      this.message = message;
    }
  }

  public static final class World {
    public int id;
    public int randomNumber;

    public World(int id, int randomNumber) {
      this.id = id;
      this.randomNumber = randomNumber;
    }
  }
}
