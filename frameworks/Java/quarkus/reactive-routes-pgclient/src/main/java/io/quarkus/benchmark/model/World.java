package io.quarkus.benchmark.model;

public class World implements Comparable<World> {

    private final Integer id;
    private Integer randomNumber;

    public World(final Integer id, final Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public Integer getId() {
        return id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(final Integer randomNumber) {
        this.randomNumber = randomNumber;
    }

    @Override
    public int compareTo(final World o) {
        return id.compareTo(o.id);
    }
}