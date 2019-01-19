/**
 * 
 */
package io.sinistral.models;

import java.util.Random;

/**
 * @author jbauer
 *
 */
public final class Fortune implements Comparable<Fortune> {

  
	  public final int id;
	  public final String message;

 

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}
 
 

	public int compareTo(Fortune o) {
		return this.message.compareTo(o.message);
	}

}