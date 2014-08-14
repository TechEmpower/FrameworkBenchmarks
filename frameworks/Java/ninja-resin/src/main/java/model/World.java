package model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class World {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    public int id;
    public int randomNumber;
}
