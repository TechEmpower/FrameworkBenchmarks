package models;

import java.io.Serializable;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Indexed;

public class Fortune  implements Serializable, Comparable<Fortune> {
	private static final long serialVersionUID = 3493429313579555024L;

    @Id
    protected ObjectId objectId;
	
	@Indexed(unique=true)
	private long id;
	
	private String message;
	
	public Fortune(){
	}
	
	public Fortune(long id, String message) {
		this.id = id;
		this.message = message;
 	}

	public String getMessage() {
		return message;
	}
	
	public long getId() {
		return id;
	}

	@Override
    public int compareTo(Fortune other) {
        return message.compareTo(other.message);
    }
}
