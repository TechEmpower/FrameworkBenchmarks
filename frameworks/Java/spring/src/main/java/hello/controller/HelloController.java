package hello.controller;

import static java.util.Comparator.comparing;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public final class HelloController {

  @Autowired
  private DbRepository dbRepository;

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
      dbRepository.updateWorld(world, randomWorldNumber());
    }
    return worlds;
  }

  @RequestMapping("/fortunes")
  @ModelAttribute("fortunes")
  List<Fortune> fortunes() {
    var fortunes = dbRepository.fortunes();

    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(comparing(fortune -> fortune.message));
    return fortunes;
  }

  private World randomWorld() {
    return dbRepository.getWorld(randomWorldNumber());
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

}
