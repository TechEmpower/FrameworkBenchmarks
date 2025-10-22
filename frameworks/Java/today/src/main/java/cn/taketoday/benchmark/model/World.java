package cn.taketoday.benchmark.model;

import java.util.Objects;

import infra.persistence.Column;
import infra.persistence.Id;
import infra.persistence.Table;

@Table("world")
public class World implements Comparable<World> {

  @Id
  private Integer id;

  @Column("randomNumber")
  private Integer randomNumber;

  public World() {
  }

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

  @Override
  public int compareTo(World o) {
    return Integer.compare(id, o.id);
  }

}
