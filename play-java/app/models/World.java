package models;

import javax.persistence.*;

import play.db.ebean.*;

@Entity
public class World extends Model {

    @Id
    public Long id;

    @Column(name = "randomNumber")
    public Long randomNumber;

    public static Finder<Long, World> find = new Finder<Long, World>(
            Long.class, World.class
    );

}