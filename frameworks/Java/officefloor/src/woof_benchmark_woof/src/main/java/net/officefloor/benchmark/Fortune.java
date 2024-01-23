package net.officefloor.benchmark;

/**
 * Fortune entry.
 * 
 * @author Daniel Sagenschneider
 */
public class Fortune {

	public final int id;

	public final String message;

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}
}