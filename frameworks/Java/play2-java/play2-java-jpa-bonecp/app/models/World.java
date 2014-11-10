package models;

import play.db.jpa.JPA;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.List;

@Entity
public class World {

    @Id
    public Long id;

    @Column(name = "randomNumber")
    public Long randomNumber;

    public static World findById(final Long id) throws Throwable {
        return JPA.withTransaction("default", true, new play.libs.F.Function0<World>() {
            public World apply() {
                return JPA.em().find(World.class, id);
            }
        });
    }

    public static List<World> save(final List<World> worlds) throws Throwable {
        for (final World world : worlds) {
            JPA.withTransaction("default", false, new play.libs.F.Function0<World>() {
                public World apply() {
                    return JPA.em().merge(world);
                }
            });
        }

        return worlds;
    }
}