package cn.taketoday.benchmark.model;

import java.util.Objects;

import cn.taketoday.jdbc.persistence.Column;
import cn.taketoday.jdbc.persistence.Id;
import cn.taketoday.jdbc.persistence.Table;

@Table("world")
public class World {

  @Id
  private Integer id;

  @Column("randomNumber")
  private Integer randomNumber;

  public World() { }

  public World(Integer id, Integer randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public Integer getId() {
    return id;
  }

  public Integer getRandomNumber() {
    return randomNumber;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public void setRandomNumber(Integer randomNumber) {
    this.randomNumber = randomNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (!(o instanceof World world))
      return false;
    return Objects.equals(id, world.id)
            && Objects.equals(randomNumber, world.randomNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, randomNumber);
  }
}
