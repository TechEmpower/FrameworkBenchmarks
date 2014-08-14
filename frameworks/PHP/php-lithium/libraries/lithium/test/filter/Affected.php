<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test\filter;

use lithium\test\Unit;
use lithium\core\Libraries;
use lithium\analysis\Inspector;

/**
 * The `Affected` test filter adds test cases to the tests that are about to be run.
 *
 * Affected test cases are determined by:
 *
 * 1. Looking at the subject of a test case.
 * 2. Searching the class tree for any classes that directly depend on that subject.
 * 3. Assigning test cases to those classes.
 */
class Affected extends \lithium\test\Filter {

	protected static $_cachedDepends = array();

	/**
	 * Takes an instance of an object (usually a Collection object) containing test
	 * instances. Adds affected tests to the test collection.
	 *
	 * @param object $report Instance of Report which is calling apply.
	 * @param array $tests The test to apply this filter on
	 * @param array $options Not used.
	 * @return object Returns the instance of `$tests`.
	 */
	public static function apply($report, $tests, array $options = array()) {
		$affected = array();
		$testsClasses = $tests->map('get_class', array('collect' => false));

		foreach ($tests as $test) {
			$affected = array_merge($affected, self::_affected($test->subject()));
		}
		$affected = array_unique($affected);

		foreach ($affected as $class) {
			$test = Unit::get($class);

			if ($test && !in_array($test, $testsClasses)) {
				$tests[] = new $test();
			}
			$report->collect(__CLASS__, array($class => $test));
		}
		return $tests;
	}

	/**
	 * Analyzes the results of a test run and returns the result of the analysis.
	 *
	 * @param object $report The report instance running this filter and aggregating results
	 * @param array $options
	 * @return array The results of the analysis.
	 */
	public static function analyze($report, array $options = array()) {
		$analyze = array();
		foreach ($report->results['filters'][__CLASS__] as $result) {
			foreach ($result as $class => $test) {
				$analyze[$class] = $test;
			}
		}

		return $analyze;
	}

	/**
	 * Returns all classes directly depending on a given class.
	 *
	 * @param string $dependency The class name to use as a dependency.
	 * @param string $exclude Regex path exclusion filter.
	 * @return array Classes having a direct dependency on `$dependency`. May contain duplicates.
	 */
	protected static function _affected($dependency, $exclude = null) {
		$exclude = $exclude ?: '/(tests|webroot|resources|libraries|plugins)/';
		$classes = Libraries::find(true, compact('exclude') + array('recursive' => true));
		$dependency = ltrim($dependency, '\\');
		$affected = array();

		foreach ($classes as $class) {
			if (isset(static::$_cachedDepends[$class])) {
				$depends = static::$_cachedDepends[$class];
			} else {
				$depends = Inspector::dependencies($class);
				$depends = array_map(function($c) { return ltrim($c, '\\'); }, $depends);
				static::$_cachedDepends[$class] = $depends;
			}

			if (in_array($dependency, $depends)) {
				$affected[] = $class;
			}
		}
		return $affected;
	}
}

?>