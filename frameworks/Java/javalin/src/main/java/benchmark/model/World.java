package benchmark.model;

public class World {

    private int id;
    private int randomNumber;

    public World() {}

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }

    @Override
    public String toString() {
        return "World{" +
                "id=" + id +
                ", randomNumber=" + randomNumber +
                '}';
    }
}
