package models;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.Id;

import io.ebean.Finder;
import io.ebean.Model;

@Entity
public class Fortune extends Model {

    private static final Finder<Long, Fortune> find = new Finder<>(Fortune.class);

    @Id
    public Long id = 0L;

    public String message;

    public Fortune() {
    }

    public Fortune(final String message) {
        this.message = message;
    }

    public static List<Fortune> findAll() {
        return find.all();
    }
}
