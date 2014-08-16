<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test;

/**
 * `Filter` is the base class for all test filters.
 */
abstract class Filter extends \lithium\core\StaticObject {

	/**
	 * Takes an instance of an object (usually a Collection object) containing test
	 * instances. Allows for preparing tests before they are run.
	 *
	 * @param object $report Instance of Report which is calling apply.
	 * @param array $tests The test to apply this filter on
	 * @param array $options Options for how this filter should be applied.
	 * @return object Returns the instance of `$tests`.
	 */
	public static function apply($report, $tests, array $options = array()) {}

	/**
	 * Analyzes the results of a test run and returns the result of the analysis.
	 *
	 * @param object $report The report instance running this filter and aggregating results
	 * @param array $options
	 * @return array The results of the analysis.
	 */
	public static function analyze($report, array $options = array()) {
		return $report->results['filters'][get_called_class()];
	}

	/**
	 * Returns data to be output by a reporter.
	 *
	 * @param string $format I.e. `'html'` or `'text'`.
	 * @param array $analysis The results of the analysis.
	 * @return string
	 */
	public static function output($format, $analysis) {}
}

?>