package models;

import play.db.ebean.Model;

import javax.persistence.Entity;
import javax.persistence.Id;

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

    public static Finder<Long, Fortune> find = new Finder<Long, Fortune>(
            Long.class, Fortune.class
    );
}