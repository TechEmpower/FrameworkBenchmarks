package models;

import com.avaje.ebean.Ebean;
import io.ebean.Model;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.List;

@Entity
public class Fortune extends Model {

    public static final Finder<Long, Fortune> find = new Finder<>(Fortune.class);

    @Id
    public Long id = 0L;

    public String message;

    public Fortune() {
    }

    public Fortune(String message) {
        this.message = message;
    }

    public static List<Fortune> findAll() {
        return Fortune.find.findList();
    }
}
