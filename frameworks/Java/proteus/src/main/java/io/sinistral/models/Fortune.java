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

	public static final String[] TMP_MESSAGES = new String[]{
			"A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1",
			"A computer program does what you tell it to do, not what you want it to do.",
			"A list is only as strong as its weakest link. — Donald Knuth",
			"A computer scientist is someone who fixes things that aren&apos;t broken.",
			"&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;",
			"Additional fortune added at request time.",
			"Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen",
			"フレームワークのベンチマーク",
			"Computers make very fast, very accurate mistakes."
	};
	
	public static final Random random = new Random();
	
	public static Fortune generate()
	{
		return new Fortune( random.nextInt(10000)+1, TMP_MESSAGES[random.nextInt(TMP_MESSAGES.length-1)] );
	}
	
	
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