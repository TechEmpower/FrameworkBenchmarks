package hello.model;

import javax.persistence.Entity;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Document
@Entity
public final class Fortune {
	@Id
	@javax.persistence.Id
	public int id;
	@Field("message")
	public String message;

	protected Fortune() {
	}

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	public int getId() {
		return id;
	}

	public String getMessage() {
		return message;
	}
}