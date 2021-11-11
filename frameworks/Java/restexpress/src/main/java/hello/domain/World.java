package hello.domain;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Indexed;

import com.strategicgains.repoexpress.domain.Identifiable;
import com.strategicgains.repoexpress.domain.Identifier;

@Entity(value = "world", noClassnameStored = true)
public class World implements Identifiable {
	@Id
	private ObjectId oid;

	@Indexed(unique = true)
	private Long id;
	private int randomNumber;

	public World() {
		super();
	}

	public World(Long id, int randomNumber) {
		this.id = id;
		this.randomNumber = randomNumber;
	}

	public Identifier getId() {
		return (id == null ? null : new Identifier(id));
	}

	public void setId(Identifier id) {
		this.id = (id == null ? null : (Long) id.primaryKey());
	}

	public int getRandomNumber() {
		return this.randomNumber;
	}
}