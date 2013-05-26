package hello.domain;

import org.bson.types.ObjectId;

import com.github.jmkgreen.morphia.annotations.Entity;
import com.github.jmkgreen.morphia.annotations.Id;
import com.github.jmkgreen.morphia.annotations.Indexed;
import com.strategicgains.repoexpress.domain.Identifiable;

@Entity(value="world", noClassnameStored=true)
public class World
implements Identifiable
{
	@Id
	private ObjectId oid;
	
	@Indexed(unique=true)
	private Long id;
	private int randomNumber;

	public World()
	{
		super();
	}

	public World(Long id, int randomNumber)
	{
		this.id = id;
		this.randomNumber = randomNumber;
	}

	public String getId()
	{
		return (id == null ? null : id.toString());
	}
	
	public void setId(String id)
	{
		this.id = (id == null ? null : Long.parseLong(id));
	}

	public int getRandomNumber()
	{
		return this.randomNumber;
	}
}
