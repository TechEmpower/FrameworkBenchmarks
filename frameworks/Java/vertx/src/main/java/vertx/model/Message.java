package vertx.model;

import com.julienviet.jsonsergen.Backend;
import com.julienviet.jsonsergen.JsonSerGen;
import io.vertx.codegen.annotations.DataObject;
import io.vertx.core.buffer.Buffer;

@DataObject
@JsonSerGen(backends = Backend.DSL_JSON)
public class Message {

  private String message;

  public Message(String message) {
    this.message = message;
  }

  public String getMessage() {
    return message;
  }

  public Message setMessage(String message) {
    this.message = message;
    return this;
  }

  public Buffer toJson() {
    return MessageJsonSerializer.toJsonBuffer(this);
  }
}
