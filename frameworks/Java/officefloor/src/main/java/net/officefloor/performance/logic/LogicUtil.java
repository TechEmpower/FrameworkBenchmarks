package net.officefloor.performance.logic;

/**
 * Utility methods.
 * 
 * @author Daniel Sagenschneider
 */
public class LogicUtil {

	/**
	 * All access via static methods.
	 */
	private LogicUtil() {
	}

	/**
	 * Generates the random number within the range.
	 * 
	 * @return Random number within the range.
	 */
	public static int generateRandomNumber(int start, int end) {
		double randomValue = Math.random();
		int randomNumber = (int) (start + Math.round(randomValue
				* (end - start)));
		return randomNumber;
	}

}