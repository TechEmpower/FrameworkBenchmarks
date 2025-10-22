package cn.taketoday.benchmark.model;

import java.util.Objects;

import infra.persistence.Id;
import infra.persistence.Table;

@Table("fortune")
public class Fortune {

  @Id
  private Integer id;

  private String message;

  public Fortune() {
  }

  public Fortune(Integer id, String message) {
    this.id = id;
    this.message = message;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  public Integer getId() {
    return id;
  }

  public String getMessage() {
    return message;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (!(o instanceof Fortune fortune))
      return false;
    return Objects.equals(id, fortune.id)
            && Objects.equals(message, fortune.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, message);
  }

}
