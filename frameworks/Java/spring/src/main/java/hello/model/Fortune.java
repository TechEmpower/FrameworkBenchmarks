package hello.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Document
public final class Fortune implements Comparable<Fortune>{

	@Id
	public final int id;

	@Field("message")
	public final String message;

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	@Override
	public int compareTo(final Fortune other) {
		return message.compareTo(other.message);
	}
}
