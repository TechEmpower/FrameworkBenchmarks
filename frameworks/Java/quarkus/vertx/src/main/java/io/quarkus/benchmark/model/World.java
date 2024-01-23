package io.quarkus.benchmark.model;

public final class World implements Comparable<World> {
    private final Integer id;
    private final Integer randomNumber;

    public World(final Integer id, final Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public Integer getId() {
        return id;
    }

    public Integer getRandomNumber() {
        return randomNumber;
    }

    @Override
    public int compareTo(final World o) {
        return id.compareTo(o.id);
    }
}