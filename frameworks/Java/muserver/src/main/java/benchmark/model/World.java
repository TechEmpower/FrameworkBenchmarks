package benchmark.model;

import java.util.Objects;

public record World(
        int id,
        int randomNumber
) {
    public World copy(Integer id, Integer randomNumber) {
        return new World(Objects.requireNonNullElse(id, this.id), Objects.requireNonNullElse(randomNumber, this.randomNumber));
    }
}
