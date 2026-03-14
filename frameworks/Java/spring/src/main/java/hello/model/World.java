package hello.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

@Document
@Table
public final class World {

	@Id
	public int id;

	@Field("randomNumber")
	@Column("randomnumber")
	public int randomNumber;


	public World(int id, int randomNumber) {
		this.id = id;
		this.randomNumber = randomNumber;
	}

}