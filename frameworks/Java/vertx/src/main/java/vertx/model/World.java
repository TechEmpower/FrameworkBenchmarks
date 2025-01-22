package vertx.model;

import com.julienviet.jsonsergen.Backend;
import com.julienviet.jsonsergen.JsonSerGen;
import io.vertx.codegen.annotations.DataObject;
import io.vertx.core.buffer.Buffer;

/**
 * The model for the "world" database table.
 */
@DataObject
@JsonSerGen(backends = Backend.DSL_JSON)
public final class World implements Comparable<World> {

  private final int id;
  private final int randomNumber;

  /**
   * Constructs a new world object with the given parameters.
   *
   * @param id the ID of the world
   * @param randomNumber the random number of the world
   */
  public World(int id, int randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public int getId() {
    return id;
  }

  public int getRandomNumber() {
    return randomNumber;
  }

  @Override
  public int compareTo(World o) {
    return Integer.compare(id, o.id);
  }

  public Buffer toJson() {
    return WorldJsonSerializer.toJsonBuffer(this);
  }

  public static Buffer toJson(World[] worlds) {
    return WorldJsonSerializer.toJsonBuffer(worlds);
  }
}