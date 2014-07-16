package hellowicket.fortune;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

/**
 *
 */
@Entity
public class Fortune
{
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	public int id;
	public String message;
}
