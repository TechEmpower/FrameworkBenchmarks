package models;

import com.avaje.ebean.Ebean;
import play.db.ebean.Model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
        Set<String> updateProperties = new HashSet<>();
        updateProperties.add("randomNumber");

        for (World world : worlds) {
            Ebean.update(world, updateProperties);
        }

        return worlds;
    }

}