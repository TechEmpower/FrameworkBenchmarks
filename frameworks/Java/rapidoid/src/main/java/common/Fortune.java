package common;

import org.rapidoid.u.U;

public class Fortune implements Comparable<Fortune> {

	private int id;
	private String message;

	public Fortune() {
	}

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	public int getId() {
		return id;
	}

	public Fortune setId(int id) {
		this.id = id;
		return this;
	}

	public String getMessage() {
		return message;
	}

	public Fortune setMessage(String message) {
		this.message = message;
		return this;
	}

	@Override
	public String toString() {
		return "Fortune{" +
			"id=" + id +
			", message='" + message + '\'' +
			'}';
	}

	@Override
	public int compareTo(Fortune o) {
		return U.compare(this.message, o.message);
	}
}
