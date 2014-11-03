package models;

import com.avaje.ebean.Ebean;
import play.db.ebean.Model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.List;

@Entity
public class World extends Model {

    @Id
    public Long id;

    @Column(name = "randomNumber")
    public Long randomNumber;

    public static Finder<Long, World> find = new Finder<Long, World>(
            Long.class, World.class
    );

    public static List<World> save(final List<World> worlds) throws Throwable {
        Ebean.save(worlds);

        return worlds;
    }

}