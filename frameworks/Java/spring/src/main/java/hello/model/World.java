package hello.model;

import jakarta.persistence.Entity;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Document
@Entity
public final class World {

	@Id
	@jakarta.persistence.Id
	public int id;
	@Field("randomNumber")
	public int randomnumber;

	protected World() {
	}

	public World(int id, int randomnumber) {
		this.id = id;
		this.randomnumber = randomnumber;
	}
}