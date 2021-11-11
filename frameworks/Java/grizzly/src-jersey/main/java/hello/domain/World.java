package hello.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class World {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private int id;
	private int randomNumber;

	public int getId() {
		return id;
	}

	public int getRandomNumber() {
		return randomNumber;
	}

	public void setRandomNumber(int randomNumber) {
		this.randomNumber = randomNumber;
	}

}
