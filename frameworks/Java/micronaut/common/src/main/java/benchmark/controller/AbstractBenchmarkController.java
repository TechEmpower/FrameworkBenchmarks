package benchmark.controller;

import benchmark.model.Fortune;
import benchmark.model.World;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class AbstractBenchmarkController {

    protected final Integer[] boxed = IntStream.range(1, 10001).boxed().toArray(Integer[]::new);

    protected List<Fortune> createFortunes() {
        List<Integer> fortuneMessages = IntStream.range(0, 10).boxed().collect(Collectors.toList());
        List<Fortune> fortunes = new ArrayList<>(fortuneMessages.size());
        for (Integer number : fortuneMessages) {
            fortunes.add(new Fortune(number + 1, "message" + number));
        }
        Collections.shuffle(fortunes);
        return fortunes;
    }

    protected List<World> createWords() {
        List<Integer> ids = new ArrayList<>(List.of(boxed));
        Collections.shuffle(ids);
        List<World> worlds = new ArrayList<>(ids.size());
        for (Integer id : ids) {
            worlds.add(new World(id, randomWorldNumber()));
        }
        return worlds;
    }

    protected Integer randomId() {
        return boxed[ThreadLocalRandom.current().nextInt(10000)];
    }

    protected Integer randomWorldNumber() {
        return boxed[ThreadLocalRandom.current().nextInt(10000)];
    }

    protected Integer parseQueryCount(String textValue) {
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
