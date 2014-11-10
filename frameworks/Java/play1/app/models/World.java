package models;

import javax.persistence.Entity;
import javax.persistence.Id;

import play.db.jpa.GenericModel;

/**
 * use a generic model as we want to explicitly define the id
 * 
 * @author tom
 * 
 */
@Entity
public class World extends GenericModel {

	public World(long i, long number) {
		id = i;
		randomNumber = number;
	}

	public World() {
	}

	@Id
	public Long id;

	public Long randomNumber;

}