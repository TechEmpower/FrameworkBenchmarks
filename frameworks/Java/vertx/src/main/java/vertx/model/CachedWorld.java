package vertx.model;

import com.julienviet.jsonsergen.Backend;
import com.julienviet.jsonsergen.JsonSerGen;
import io.vertx.codegen.annotations.DataObject;
import io.vertx.core.buffer.Buffer;

import java.util.List;

/**
 * The model for the "world" database table.
 */
@DataObject
@JsonSerGen(backends = Backend.DSL_JSON)
public final class CachedWorld implements Comparable<CachedWorld> {

  private final int id;
  private final int randomNumber;

  /**
   * Constructs a new world object with the given parameters.
   *
   * @param id the ID of the world
   * @param randomNumber the random number of the world
   */
  public CachedWorld(int id, int randomNumber) {
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
  public int compareTo(CachedWorld o) {
    return Integer.compare(id, o.id);
  }

  public static Buffer toJson(List<CachedWorld> worlds) {
    return CachedWorldJsonSerializer.toJsonBuffer(worlds);
  }
}