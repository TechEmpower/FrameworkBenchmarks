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

	/**
	 * Obtains the query count.
	 * 
	 * @param queries
	 *            Parameter value for <code>queries</code>.
	 * @return Query count.
	 */
	public static int getQueryCount(String queries) {

		// Obtain the number of queries to make
		int queryCount;
		try {
			queryCount = Integer.parseInt(queries);
		} catch (NumberFormatException ex) {
			return 1;
		} catch (NullPointerException ex) {
			return 1;
		}

		// Ensure within bounds
		if (queryCount < 1) {
			return 1;
		} else if (queryCount > 500) {
			return 500;
		} else {
			return queryCount;
		}
	}

}