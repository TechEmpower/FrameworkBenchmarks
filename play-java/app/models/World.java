package models;

import java.util.*;
import javax.persistence.*;

import play.db.ebean.*;
import play.data.format.*;
import play.data.validation.*;

@Entity 
public class World extends Model {

  @Id
  public Long id;
  
  @Column(name = "randomNumber")
  public Long randomNumber;
  
  public static Finder<Long,World> find = new Finder<Long,World>(
    Long.class, World.class
  ); 

}