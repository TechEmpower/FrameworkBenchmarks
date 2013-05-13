package models;

import javax.persistence.*;

import play.db.jpa.JPA;
import play.db.jpa.Transactional;

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
}