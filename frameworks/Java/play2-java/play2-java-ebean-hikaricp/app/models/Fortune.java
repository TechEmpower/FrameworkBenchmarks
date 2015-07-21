package models;

import com.avaje.ebean.Ebean;
import com.avaje.ebean.Model;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.List;

@Entity
public class Fortune extends Model {

    @Id
    public Long id = 0L;

    public String message;

    public Fortune() {
    }

    public Fortune(String message) {
        this.message = message;
    }

    public static List<Fortune> findAll() {
        return Ebean.find(Fortune.class).findList();
    }
}