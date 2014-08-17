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
public class Fortune extends GenericModel implements Comparable<Fortune> {

	@Id
	private final Long id;
	private final String message;

	public Fortune(long id, String message) {
		this.id = id;
		this.message = message;
	}

	public Long getId() {
		return this.id;
	}

	public String getMessage() {
		return this.message;
	}

	/**
	 * For our purposes, Fortunes sort by their message text.
	 */
	@Override
	public int compareTo(Fortune other) {
		return getMessage().compareTo(other.getMessage());
	}

}
