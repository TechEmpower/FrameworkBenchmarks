<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test;

use lithium\util\Set;
use lithium\core\Libraries;
use lithium\core\Environment;

/**
 * The Lithium Test Dispatcher
 *
 * This Dispatcher is used exclusively for the purpose of running, organizing and compiling
 * statistics for the built-in Lithium test suite.
 */
class Dispatcher extends \lithium\core\StaticObject {

	/**
	 * Composed classes used by the Dispatcher.
	 *
	 * @var array Key/value array of short identifier for the fully-namespaced class.
	 */
	protected static $_classes = array(
		'group' => 'lithium\test\Group',
		'report' => 'lithium\test\Report'
	);

	/**
	 * Runs a test group or a specific test file based on the passed
	 * parameters.
	 *
	 * @param string $group If set, this test group is run. If not set, a group test may
	 *        also be run by passing the 'group' option to the $options parameter.
	 * @param array $options Options array for the test run. Valid options are:
	 *        - `'case'`: The fully namespaced test case to be run.
	 *        - `'group'`: The fully namespaced test group to be run.
	 *        - `'filters'`: An array of filters that the test output should be run through.
	 *        - `'format'`: The format of the template to use, defaults to `'txt'`.
	 *        - `'reporter'`: The reporter to use.
	 * @return array A compact array of the title, an array of the results, as well
	 *         as an additional array of the results after the $options['filters']
	 *         have been applied.
	 * @filter
	 */
	public static function run($group = null, array $options = array()) {
		$defaults = array(
			'title' => $group,
			'filters' => array(),
			'format' => 'txt',
			'reporter' => null
		);
		$options += $defaults;
		$isCase = is_string($group) && preg_match('/Test$/', $group);
		$items = ($isCase) ? array(new $group()) : (array) $group;

		$options['filters'] = Set::normalize($options['filters']);
		$group = static::_group($items);
		$report = static::_report($group, $options);

		return static::_filter(__FUNCTION__, compact('report'), function($self, $params, $chain) {
			$environment = Environment::get();
			Environment::set('test');

			$params['report']->run();

			Environment::set($environment);
			return $params['report'];
		});
	}

	/**
	 * Creates the group class based
	 *
	 * @see lithium\test\Dispatcher::$_classes
	 * @param array $data Array of cases or groups.
	 * @return object Group object constructed with `$data`.
	 */
	protected static function _group($data) {
		$group = Libraries::locate('test', static::$_classes['group']);
		$class = static::_instance($group, compact('data'));
		return $class;
	}

	/**
	 * Creates the test report class based on either the passed test case or the
	 * passed test group.
	 *
	 * @see lithium\test\Dispatcher::$_classes
	 * @param string $group
	 * @param array $options Options array passed from Dispatcher::run(). Should contain
	 *        one of 'case' or 'group' keys.
	 * @return object Group object constructed with the test case or group passed in $options.
	 */
	protected static function _report($group, $options) {
		$report = Libraries::locate('test', static::$_classes['report']);
		$class = static::_instance($report, compact('group') + $options);
		return $class;
	}
}

?>