package hello.model;


import com.hellokaton.anima.Model;
import com.hellokaton.anima.annotation.Table;

@Table(name = "fortune")
public class Fortune extends Model {

    private Integer id;
    private String  message;

    public Fortune() {
    }

    public Fortune(Integer id, String message) {
        this.id = id;
        this.message = message;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
